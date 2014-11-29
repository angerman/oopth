{-# LANGUAGE CPP, LambdaCase, BangPatterns, MagicHash, TupleSections, ScopedTypeVariables, DeriveDataTypeable #-}

{- |
     Evaluate Template Haskell splices on node.js
     Obtained 22.11.2014 from GitHub (915f263)
     Adapted to work outside of GHCJS
 -}

module OOPTH.Eval (
         runTHServer
       ) where

import           Serialized
import qualified OOPTH.Types as T

import           Control.Applicative
import qualified Control.Exception        as E
import           Control.Monad

import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as B
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Lazy     as BL
import qualified Data.ByteString.Unsafe   as BU
import           Data.Data
import           Data.Dynamic
import           Data.Int
import           Data.IORef
import           Data.Map                 (Map)
import qualified Data.Map                 as M
import           Data.Maybe
import           Data.Monoid              ((<>))
import           Data.Typeable
import           Data.Typeable.Internal
import           Data.Word

import           Foreign.C
import           Foreign.Ptr

import           GHC.Prim
import           GHC.Desugar

import qualified Language.Haskell.TH        as TH
import qualified Language.Haskell.TH.Syntax as TH

import           System.IO

import           Unsafe.Coerce

import           Control.Concurrent        (MVar, modifyMVar)
-- DyLib loading
import           OOPTH.Import              (loadQuasiActionFromLibrary)

data QState = QState { qsMap        :: Map TypeRep Dynamic    -- ^ persistent data between splices in a module
                     , qsFinalizers :: [TH.Q ()]              -- ^ registered finalizers (in reverse order)
                     , qsMessages   :: IORef [(Bool, String)] -- ^ messages reported
                     , qsLocation   :: Maybe TH.Loc           -- ^ location for current splice, if any
                     , qsRecv       :: IO T.SeqMessage
                     , qsSend       :: T.SeqMessage -> IO ()
                     , qsReqN       :: Int
                     }
instance Show QState where show _ = "<QState>"

initQState :: IO T.SeqMessage -> (T.SeqMessage -> IO ()) -> IORef [(Bool, String)] -> QState
initQState recv send msgs = QState M.empty [] msgs Nothing recv send 1

runModFinalizers :: GHCJSQ ()
runModFinalizers = go =<< getState
  where
    go s | (f:ff) <- qsFinalizers s =
      putState (s { qsFinalizers = ff}) >> TH.runQ f >> getState >>= go
    go _ = return ()

data GHCJSQ a = GHCJSQ { runGHCJSQ :: QState -> IO (a, QState) }

data GHCJSQException = GHCJSQException QState String
  deriving (Show, Typeable)

instance E.Exception GHCJSQException

instance Functor GHCJSQ where
  fmap f (GHCJSQ s) = GHCJSQ $ fmap (\(x,s') -> (f x,s')) . s

instance Applicative GHCJSQ where
  f <*> a = GHCJSQ $ \s ->
    do (f',s')  <- runGHCJSQ f s
       (a',s'') <- runGHCJSQ a s'
       return (f' a', s'')
  pure x = GHCJSQ (\s -> return (x,s))

instance Monad GHCJSQ where
  m >>= f = GHCJSQ $ \s ->
    do (m', s')  <- runGHCJSQ m s
       (a,  s'') <- runGHCJSQ (f m') s'
       return (a, s'')
  return    = pure
  fail err  = GHCJSQ $ \s -> E.throw (GHCJSQException s err)

getState :: GHCJSQ QState
getState = GHCJSQ $ \s -> return (s,s)

putState :: QState -> GHCJSQ ()
putState s = GHCJSQ $ \_ -> return ((),s)

noLoc :: TH.Loc
noLoc = TH.Loc "<no file>" "<no package>" "<no module>" (0,0) (0,0)

sendRequest :: T.Message -> GHCJSQ T.Message
sendRequest req = GHCJSQ $ \s -> do
  let reqN = qsReqN s
  TH.qRunIO $ (qsSend s) (T.ReqMessage reqN req)
  T.RespMessage reqN resp <- TH.qRunIO (qsRecv s)
  return (resp, s { qsReqN = reqN + 1 })

sendResult :: T.Message -> GHCJSQ ()
sendResult msg = GHCJSQ $ \s -> do
  TH.qRunIO $ (qsSend s) (T.ReqMessage 0 msg)
  return ((), s)

awaitMessage :: GHCJSQ T.Message
awaitMessage = GHCJSQ $ \s -> do
  T.RespMessage _ msg <- TH.qRunIO (qsRecv s)
  return (msg, s)

instance TH.Quasi GHCJSQ where
  qNewName str = do
    T.NewName' name <- sendRequest $ T.NewName str
    return name
  qReport isError msg = getState >>=
    TH.qRunIO . flip modifyIORef ((isError,msg):) . qsMessages
  qRecover (GHCJSQ h) (GHCJSQ a) = GHCJSQ $ \s -> a s
    -- discard error messages on recovery
--    liftIO (a s) `E.catch` \(GHCJSQException s' _) ->
--      liftIO (TH.qRunIO (modifyIORef (qsMessages s') (filter (not . fst))) >> h s')
  qLookupName isType occ = do
    T.LookupName' name <- sendRequest $ T.LookupName isType occ
    return name
  qReify name = do
    T.Reify' info <- sendRequest $ T.Reify name
    return info
  qReifyInstances name tys = do
    T.ReifyInstances' decls <- sendRequest $ T.ReifyInstances name tys
    return decls
  qReifyRoles name = do
    T.ReifyRoles' roles <- sendRequest $ T.ReifyRoles name
    return roles
  qReifyAnnotations lookup = do
    T.ReifyAnnotations' payloads <- sendRequest $ T.ReifyAnnotations lookup
    return (convertAnnPayloads payloads)
  qReifyModule m = do
    T.ReifyModule' mi <- sendRequest $ T.ReifyModule m
    return mi
  qLocation = fromMaybe noLoc . qsLocation <$> getState
  qRunIO m = GHCJSQ $ \s -> fmap (,s) m
  qAddDependentFile file = do
    T.AddDependentFile' <- sendRequest $ T.AddDependentFile file
    return ()
  qAddTopDecls decls = do
    T.AddTopDecls' <- sendRequest $ T.AddTopDecls decls
    return ()
  qAddModFinalizer fin = GHCJSQ $ \s ->
    return ((), s { qsFinalizers = fin : qsFinalizers s })
  qGetQ = GHCJSQ $ \s ->
    let lookup :: forall a. Typeable a => Map TypeRep Dynamic -> Maybe a
        lookup m = fromDynamic =<< M.lookup (typeOf (undefined::a)) m
    in return (lookup (qsMap s), s)
  qPutQ k = GHCJSQ $ \s ->
    return ((), s { qsMap = M.insert (typeOf k) (toDyn k) (qsMap s) })

makeAnnPayload :: forall a. Data a => a -> ByteString
makeAnnPayload x =
  let TypeRep (Fingerprint w1 w2) _ _ = typeOf (undefined :: a)
      fp = runPut (putWord64be w1 >> putWord64be w2)
  in  BL.toStrict $ fp <> BL.pack (serializeWithData x)

convertAnnPayloads :: forall a. Data a => [ByteString] -> [a]
convertAnnPayloads bs = catMaybes (map convert bs)
  where
    TypeRep (Fingerprint w1 w2) _ _ = typeOf (undefined :: a)
    getFp b = runGet ((,) <$> getWord64be <*> getWord64be) $ BL.fromStrict (B.take 16 b)
    convert b | (bw1,bw2) <- getFp b, bw1 == w1, bw2 == w2 =
                  Just (deserializeWithData . B.unpack . B.drop 16 $ b)
              | otherwise = Nothing

-- | the Template Haskell server (Courier Edition)
runTHServer :: MVar Int -- ^ the mvar to extract the server number from.
            -> IO T.SeqMessage -- ^ the receive action, should dispatch a message or block until one is available.
            -> (T.SeqMessage -> IO ()) -- ^ the send action, takes a message and returns nothing
            ->  IO ()
runTHServer mi recv send = do
  msgs <- newIORef []
  -- get a new server number.
  serverN <- modifyMVar mi $ \i -> return (i+1,i)
  putStrLn $ "[Server] Starting..."
  void (runGHCJSQ (server serverN 0) (initQState recv send msgs)) {-`E.catches`
    [ E.Handler $ \(GHCJSQException _ msg) -> sendReportedMessages' msgs >> void (sendRequest $ T.QFail msg)
    , E.Handler $ \(E.SomeException e)     -> sendReportedMessages' msgs >> void (sendRequest $ T.QException (show e))
    ]-}
  where
--    server :: GHCJSQ a
    log :: String -> GHCJSQ ()
    log = TH.qRunIO . putStrLn
    -- server takes a server and an iternation number.
    -- server :: Int -> Int -> IO (a, QState)
    server n i = do
      log "[Server] Waiting for Message"
      msg <- awaitMessage
      log "[Server] Received Message"
      case msg of
        T.RunTH t code loc -> do
          log "[Server] Loading code..."
          a <- TH.qRunIO (loadCode n i code)
          log "[Server] Running TH..."
          runTH t a loc
          log "[Server] Reporting Messages..."
          sendReportedMessages
          log "[Server] Recur..."
          server n (i + 1)
        T.FinishTH -> do
          runModFinalizers
          sendReportedMessages
          sendResult T.FinishTH'
        _ -> error "runTHServer: unexpected message type"

{-# NOINLINE runTH #-}
runTH :: T.THResultType -> Any -> Maybe TH.Loc -> GHCJSQ ()
runTH rt obj = \mb_loc -> obj `seq` do
  s0 <- getState
  putState $ s0 { qsLocation = mb_loc }
  res <- case rt of
           T.THExp        -> runTHCode (unsafeCoerce obj :: TH.Q TH.Exp)
           T.THPat        -> runTHCode (unsafeCoerce obj :: TH.Q TH.Pat)
           T.THType       -> runTHCode (unsafeCoerce obj :: TH.Q TH.Type)
           T.THDec        -> runTHCode (unsafeCoerce obj :: TH.Q [TH.Dec])
           T.THAnnWrapper -> case unsafeCoerce obj of
                             AnnotationWrapper x -> return (makeAnnPayload x)
  sendReportedMessages
  s1 <- getState
  sendResult $ T.RunTH' res
  putState $ s1 { qsLocation = Nothing }

{-# NOINLINE runTHCode #-}
runTHCode :: Binary a => TH.Q a -> GHCJSQ ByteString
runTHCode c = BL.toStrict . runPut . put <$> TH.runQ c

{-# NOINLINE loadCode #-}
loadCode :: Int -> Int -> ByteString -> IO Any
loadCode serverN iterN bs = do
  let lib = "code_" ++ (show serverN) ++ "_" ++ (show iterN) ++ ".dylib"
  putStrLn $ "[Server] Writing lib (" ++ lib ++ ")..."
  _ <- B.writeFile  lib bs
  putStrLn $ "[Server] Loading Action..."
  a <- loadQuasiActionFromLibrary lib
  putStrLn $ "[Server] Returning Action..."
  return a

sendReportedMessages :: GHCJSQ ()
sendReportedMessages =
  getState >>= sendReportedMessages' . qsMessages

sendReportedMessages' :: IORef [(Bool,String)] -> GHCJSQ ()
sendReportedMessages' r =
  let report (isError, msg) = do
        T.Report' <- sendRequest $ T.Report isError msg
        return ()
  in  TH.qRunIO (atomicModifyIORef r (\msgs -> ([], msgs))) >>= mapM_ report . reverse

-- fmap (runGet get . BL.fromStrict) . toBs =<< js_awaitMessage

-- | send result back
-- sendResult :: Message -> IO ()
-- sendResult msg = undefined
{-
do
  let bs = BL.toStrict $ runPut (put msg)
  p <- fromBs bs
  js_sendMessage p (B.length bs)

-}
-- | send a request and wait for the response
-- sendRequest :: Message -> IO Message
-- sendRequest msg = undefined

{-
do
  let bs = BL.toStrict $ runPut (put msg)
  p <- fromBs bs
  fmap (runGet get . BL.fromStrict) . toBs =<< js_sendRequest p (B.length bs)
-}

{-
foreign import javascript interruptible "h$TH.sendRequest($1_1,$1_2,$2,$c);"
  js_sendRequest :: Ptr Word8 -> Int -> IO (Ptr Word8)

foreign import javascript interruptible "h$TH.sendMessage($1_1,$1_2,$2,0,$c);"
  js_sendMessage :: Ptr Word8 -> Int -> IO ()

foreign import javascript interruptible "h$TH.awaitMessage(0,$c);"
  js_awaitMessage :: IO (Ptr Word8)

foreign import javascript unsafe "h$TH.bufSize($1_1, $1_2)"
  js_bufSize :: Ptr Word8 -> IO Int

-- | actually returns the heap object to be evaluated
foreign import javascript unsafe "h$TH.loadCode($1_1,$1_2,$2)"
  js_loadCode :: Ptr Word8 -> Int -> IO Double

-- | only safe in JS
fromBs :: ByteString -> IO (Ptr Word8)
fromBs bs = BU.unsafeUseAsCString bs (return . castPtr)

-- | build a ByteString that uses the whole buffer, only works in JS
toBs :: Ptr Word8 -> IO ByteString
toBs p = do
  l <- js_bufSize p
  BU.unsafePackCStringLen (castPtr p, l)
-}
