{-# LANGUAGE CPP, OverloadedStrings, LambdaCase, TupleSections, ScopedTypeVariables #-}
module OutOfProcTH (installOOPTHHook) where

import OOPTH.Settings
import OOPTH.Hacks

import OutOfProcTHBuilder ( buildDynamicLib )

-- Hooks
import DynFlags
import Hooks
import BasicTypes
import qualified OOPTH.Types                    as TH
import qualified Language.Haskell.TH            as TH
import           Language.Haskell.TH.Syntax     (Quasi)
import qualified Language.Haskell.TH.Syntax     as TH
import           Data.ByteString                (ByteString)
import           Data.Text                      (Text)
import           Control.Monad
import           Data.Monoid
import           Control.Applicative            ((<$>), pure)
import           Control.Concurrent
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put

import           GHC.Desugar

import qualified Data.ByteString                as B
import qualified Data.ByteString.Lazy           as BL
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import qualified Data.Text.IO                   as T

import HscTypes
import Unsafe.Coerce
import CoreSyn
import SrcLoc
import CorePrep         ( corePrepExpr )

import BasicTypes
import Type
import RdrName
import CoreUtils
import Module
import Packages
import Name
import Id
import Unique
import RnEnv

import SimplCore
import CoreTidy
import VarEnv
import PprCore (pprCoreExpr)

-- printing
import qualified Outputable                     as O

import UniqSet
import           Control.Lens     ((^..))
import           Data.Data.Lens   (template)
import qualified Data.Map                       as M

import Maybes

-- running the runner
import System.Process (createProcess, cwd, proc, waitForProcess)
import System.Exit (ExitCode(..))
import System.IO (hFlush, stdout, hPutStrLn, stderr)
-- reading the value from the file
-- import Binary
import Serialized

import Network.Endpoints
import Network.Transport.TCP

installOOPTHHook :: DynFlags -> DynFlags
installOOPTHHook dyflags = dyflags { hooks = addHooks (hooks dyflags) }
  where
    addHooks h = h { hscCompileCoreExprHook = Just compileCoreExpr }

myResult :: HValue
myResult = unsafeCoerce myExampleExpr

myExampleExpr :: TH.Q TH.Exp
myExampleExpr = return (TH.ConE (TH.mkName "Nothing"))

{-
readValueFromFile :: FilePath -> IO (TH.Q TH.Exp)
readValueFromFile f = do
  h   <- readBinMem f
  dat <- get h
  let Just value = fromSerialized deserializeWithData dat :: Maybe TH.Exp
  return . return $ value
-}
{-
readHValueFromRunner :: FilePath -> IO HValue
readHValueFromRunner fin = do
  let base   = "/Users/angerman/Projects/haskell.org/oopth/"
      runner = base ++ "Runner2"
      fin'   = base ++ fin
      fout   = fin' ++ ".result"
      args   = [fin', fout]
  putStrLn $ "Creating Process: " ++ runner ++ " with args " ++ (show args)
  hFlush stdout
  (_,_,_,h) <- createProcess (proc runner args) -- { cwd = mdir }
  ec <- waitForProcess h
  putStrLn $ "Done!"
  case ec of
    ExitSuccess -> unsafeCoerce $ readValueFromFile fout
    ExitFailure i -> panic $ "Failed to run runner! " ++ show i
-}
--------------------------------------------------------------------------------
-- Data Types (imported from GhcJS)

-- | run some TH code, start a runner if necessary
runTh :: forall m. Quasi m
      => Bool
      -> GhcjsEnv
      -> HscEnv
      -> DynFlags
      -> [PackageId]
      -> Type.Type       -- ^ type of the result
      -> ByteString -- ^ in-memory object of the compiled CoreExpr
      -> Text       -- ^ JavaScript symbol name that the expression is bound to
      -> m HValue
runTh is_io js_env hsc_env dflags expr_pkgs ty code symb = do
--  qRunIO $ putStrLn "[runTh] Begin"
  loc <- if is_io then return Nothing
                  else Just <$> TH.qLocation
  let m   = maybe "<global>" TH.loc_module loc
      sty = show ty
      toHv :: Show a => Get a -> ByteString -> m HValue
      toHv g b = let h = runGet g (BL.fromStrict b)
                 in  {- TH.qRunIO (print h) >> -} return (unsafeCoerce h)
      getAnnWrapper :: ByteString -> m HValue
      getAnnWrapper bs = return (unsafeCoerce $ AnnotationWrapper (B.unpack bs))
      convert
        | sty == "Language.Haskell.TH.Syntax.Q Language.Haskell.TH.Syntax.Exp"
            = Just (TH.THExp,  toHv (get :: Get TH.Exp))
        | sty == "Language.Haskell.TH.Syntax.Q [Language.Haskell.TH.Syntax.Dec]"
            = Just (TH.THDec,  toHv (get :: Get [TH.Dec]))
        | sty == "Language.Haskell.TH.Syntax.Q Language.Haskell.TH.Syntax.Pat"
            = Just (TH.THPat,  toHv (get :: Get TH.Pat))
        | sty == "Language.Haskell.TH.Syntax.Q Language.Haskell.TH.Lib.Type"
            = Just (TH.THType, toHv (get :: Get TH.Type))
        | sty == "GHC.Desugar.AnnotationWrapper"
            = Just (TH.THAnnWrapper, getAnnWrapper)
        | otherwise = Nothing
--  qRunIO $ putStrLn "[runTh] Convert"
  case convert of
    Nothing -> error ("runTh: unexpected Template Haskell expression type: " ++ sty)
    Just (tht, getHv) -> do
--      qRunIO $ putStrLn "[runTh / Just _ _] Getting Runner"
      r <- getThRunner is_io dflags js_env hsc_env m
--      base <- TH.qRunIO $ takeMVar (thrBase r)
--      let settings = thSettings { gsUseBase = BaseState base }
--      lr  <- TH.qRunIO $ linkTh settings [] dflags expr_pkgs (hsc_HPT hsc_env) (Just code)
--      ext <- TH.qRunIO $ mconcat <$> mapM (Gen2.tryReadShimFile dflags) (Gen2.linkLibB lr ++ Gen2.linkLibA lr)
--      let bs = ext <> BL.toStrict (Gen2.linkOut lr)
--                   <> T.encodeUtf8 ("\nh$TH.loadedSymbol = " <> symb <> ";\n")
      let bs = code
--      qRunIO $ putStrLn "[runTh / Just _ _] Requesting Runner"
      hv <- requestRunner is_io r (TH.RunTH tht bs loc) >>= \case
              TH.RunTH' bsr -> getHv bsr
              _             -> error "runTh: unexpected response, expected RunTH' message"
--      TH.qRunIO $ putMVar (thrBase r) (Gen2.linkBase lr)
      TH.qRunIO $ putStrLn "[runTh / Just _ _] Received Result. Done."
      return hv

-- | instruct the runner to finish up
finishTh :: Quasi m => Bool -> GhcjsEnv -> String -> ThRunner -> m ()
finishTh is_io js_env m runner = do
    --let ph = thrProcess runner
    --TH.qRunIO $ takeMVar (thrBase runner)
    requestRunner is_io runner TH.FinishTH >>= \case
      TH.FinishTH' -> return ()
      _            -> error "finishTh: unexpected response, expected FinishTH' message"
    --TH.qRunIO $ modifyMVar_ (thRunners js_env) (return . M.delete m)
    --TH.qRunIO $ maybe (void $ terminateProcess ph) (\_ -> return ()) =<< timeout 30000000 (waitForProcess ph)

thSettings :: GhcjsSettings
thSettings = GhcjsSettings False True False False Nothing Nothing Nothing True True True Nothing NoBase

getThRunner :: Quasi m => Bool -> DynFlags -> GhcjsEnv -> HscEnv -> String -> m ThRunner
getThRunner is_io dflags js_env hsc_env m = do
  runners <- TH.qRunIO $ takeMVar (thRunners js_env)
  case M.lookup m runners of
    Just r  -> TH.qRunIO (putMVar (thRunners js_env) runners) >> return r
    Nothing -> do
      r <- TH.qRunIO $ do
          let master = "master"
              slave  = "slave"
              resolver = resolverFromList [(master, "localhost:2001"),
                                           (slave,  "localhost:2000")]
          transport <- newTCPTransport resolver
          endpoint <- newEndpoint [transport]
          Right () <- bindEndpoint endpoint master
          return $ ThCourierRunner (receiveMessage endpoint) (sendMessage_ endpoint slave)
          
      when (not is_io) $ TH.qAddModFinalizer (TH.Q $ finishTh is_io js_env m r)
      TH.qRunIO $ putMVar (thRunners js_env) (M.insert m r runners)
      return r

sendToRunner :: ThRunner -> Int -> TH.Message -> IO ()
sendToRunner runner responseTo msg =
  sendToRunnerRaw runner responseTo (BL.toStrict . runPut . put $ msg)

sendToRunnerRaw :: ThRunner -> Int -> ByteString -> IO ()
sendToRunnerRaw runner responseTo bs = do
  (thrSend runner) bs

requestRunner :: Quasi m => Bool -> ThRunner -> TH.Message -> m TH.Message
requestRunner is_io runner msg = TH.qRunIO (sendToRunner runner 0 msg) >> res
  where
    res = TH.qRunIO (readFromRunner runner) >>= \case
      (msg, 0) -> return msg
      (req, n) -> handleRunnerReq is_io runner req >>= TH.qRunIO . sendToRunner runner n >> res

readFromRunner :: ThRunner -> IO (TH.Message, Int)
readFromRunner runner = do
  putStrLn "Reading..."
  payload <- thrRecv runner
  let msg = runGet get (BL.fromStrict payload)
  putStrLn "...Read"
  return (msg, 0)

handleRunnerReq :: Quasi m => Bool -> ThRunner -> TH.Message -> m TH.Message
handleRunnerReq is_io runner msg = case msg of
  TH.NewName n           -> TH.NewName'                       <$> TH.qNewName n
  TH.QException e        -> {- term                              >> -} error e
  TH.QFail e             -> {- term                              >> -} fail e
  TH.Report isErr msg    -> TH.qReport isErr msg              >>  pure TH.Report'
  TH.LookupName b n      -> TH.LookupName'                    <$> TH.qLookupName b n
  TH.Reify n             -> TH.Reify'                         <$> TH.qReify n
  TH.ReifyInstances n ts -> TH.ReifyInstances'                <$> TH.qReifyInstances n ts
  TH.ReifyRoles n        -> TH.ReifyRoles'                    <$> TH.qReifyRoles n
  TH.ReifyAnnotations _ | is_io -> error "qReifyAnnotations not supported in IO"
  TH.ReifyAnnotations nn -> TH.ReifyAnnotations' . map B.pack <$> unsafeReifyAnnotationsQ nn
  TH.ReifyModule m       -> TH.ReifyModule'                   <$> TH.qReifyModule m
  TH.AddDependentFile f  -> TH.qAddDependentFile f            >>  pure TH.AddDependentFile'
  TH.AddTopDecls decs    -> TH.qAddTopDecls decs              >>  pure TH.AddTopDecls'
  _                      -> {- term >> -} error "handleRunnerReq: unexpected request"
--  where
--    term = TH.qRunIO (terminateProcess $ thrProcess runner)


-- most of this is an adapation from ghcjsCompileCoreExpr (Gen2.TH from ghcjs)
compileCoreExpr :: HscEnv -> SrcSpan -> CoreExpr -> IO HValue
compileCoreExpr hsc_env srcspan ds_expr = do
  
  js_env  <- newGhcjsEnv
  -- see HscMain.hs:1653 for the default method.
  putStrLn "=== Compile Core Expr ==="
  prep_expr <- corePrepExpr dflags hsc_env ds_expr
  
  buildDynamicLib dflags ds_expr

  putStrLn "=== Contacting Runner ==="
  bs <- B.readFile "a.out"
  let r  = TH.Q (runTh isNonQ js_env hsc_env dflags (eDeps prep_expr) ty bs "Nothing")
  res <- if isNonQ
    then TH.runQ r
    else return (unsafeCoerce r)
  putStrLn "========= End ==========="

  return res
  -- TODO: for iOS (on the device)
  -- we will have to codesign
  -- the produced library.
  where
    isNonQ   = show ty == "GHC.Desugar.AnnotationWrapper"
--    symb n   = "h$thrunnerZCThRunner" <> T.pack (show n) <> "zithExpr"
    ty       = expandTypeSynonyms (exprType ds_expr)
--    thExpr n = mkVanillaGlobal (mkExternalName (mkRegSingleUnique (1+n)) (mod n) (mkVarOcc "thExpr") srcspan) ty
--    bind n e = NonRec (thExpr n) e
--    mod n    = mkModule pkg (mkModuleName $ "ThRunner" ++ show n)
--    pkg      = stringToPackageKey "thrunner"
    dflags   = hsc_dflags hsc_env
    eDeps e  = uniqSetToList . mkUniqSet . catMaybes $ map (fmap modulePackageId . nameModule_maybe . idName) (e ^.. template)


-- for some reason this doesn't work, although it seems to do the same as the code below
-- myReifyAnnotations :: TH.Quasi m => TH.AnnLookup -> m [[Word8]]
-- myReifyAnnotations = TH.qReifyAnnotations

{- NOINLINE unsafeReifyAnnotationsQ #-}
unsafeReifyAnnotationsQ :: TH.AnnLookup -> m [[Word8]]
unsafeReifyAnnotationsQ lookup = undefined -- unsafeCoerce (reifyAnnotationsTcM lookup)
{-
reifyAnnotationsTcM :: TH.AnnLookup -> TcM [[Word8]]
reifyAnnotationsTcM th_name = undefined
  name <- lookupThAnnLookup th_name
  topEnv <- getTopEnv
  epsHptAnns <- liftIO $ prepareAnnotations topEnv Nothing
  tcg <- getGblEnv
  let selectedEpsHptAnns = findAnns deserializeWithData epsHptAnns name
      selectedTcgAnns = findAnns deserializeWithData (tcg_ann_env tcg) name
  return (selectedEpsHptAnns ++ selectedTcgAnns)
-}