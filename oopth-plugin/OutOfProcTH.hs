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

import UniqFM
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


import           Network.Simple.TCP
import           Network.Service
import           Network.Transport.Encoding.Base64 ( mkService )
--------------------------------------------------------------------------------
-- Service construction
mkServiceClient :: ServiceMessage a => HostName -> ServiceName -> IO (Service a)
mkServiceClient h p = connect h p mkService
--------------------------------------------------------------------------------


type CommandLineOption = String

installOOPTHHook :: [CommandLineOption] -> DynFlags -> DynFlags
installOOPTHHook opts dyflags = dyflags { hooks = addHooks (hooks dyflags) }
  where
    addHooks h = h { hscCompileCoreExprHook = Just $ compileCoreExpr opts }

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
              m             -> error $ "runTh: unexpected response, expected RunTH' message; got " ++ (show m)
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
        service <- (mkServiceClient "127.0.0.1" "2000" :: IO (Service TH.SeqMessage))
        return $ ThServiceRunner service          
      when (not is_io) $ TH.qAddModFinalizer (TH.Q $ finishTh is_io js_env m r)
      TH.qRunIO $ putMVar (thRunners js_env) (M.insert m r runners)
      return r


sendToRunner :: ThRunner -> Int -> TH.Message -> IO ()
sendToRunner runner responseTo msg =
  sSend  (thrService runner) (TH.RespMessage responseTo msg)
--  sendToRunnerRaw runner responseTo (BL.toStrict . runPut . put $ msg)

{-
sendToRunnerRaw :: ThRunner -> Int -> ByteString -> IO ()
sendToRunnerRaw runner responseTo bs = do
  (thrSend runner) bs
-}

requestRunner :: Quasi m => Bool -> ThRunner -> TH.Message -> m TH.Message
requestRunner is_io runner msg = TH.qRunIO (sendToRunner runner 0 msg) >> res
  where
    res = TH.qRunIO (readFromRunner runner) >>= \case
      (msg, 0) -> return msg
      (req, n) -> handleRunnerReq is_io runner req >>= TH.qRunIO . sendToRunner runner n >> res

readFromRunner :: ThRunner -> IO (TH.Message, Int)
readFromRunner runner = do
  putStrLn "Reading..."
  TH.ReqMessage n msg <- sRecv (thrService runner)
  putStrLn "...Read"
  return (msg, n)

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
compileCoreExpr :: [CommandLineOption] -> HscEnv -> SrcSpan -> CoreExpr -> IO HValue
compileCoreExpr opts hsc_env srcspan ds_expr = do
  
  js_env  <- newGhcjsEnv
  -- see HscMain.hs:1653 for the default method.
  putStrLn "=== Compile Core Expr ==="
  prep_expr <- corePrepExpr dflags hsc_env ds_expr

  -- TODO: obtain a number through an MVar or alike,
  --       such that built dynamic libraries are
  --       given separete numbers.
  -- WARN: This will fail as soon as we have more
  --       than one splice!
  let n = 1

  -- Some debug information about the "linked" dependencies.
  {-
  putStrLn "=== Package Deps ========"
  putStrLn $ unwords $ map (show . packageIdString) (eDeps prep_expr)
  putStrLn "=== Module Deps ========="  
  putStrLn $ unwords $ map (show . moduleNameString) (mDeps prep_expr)
  putStrLn "=== Package Deps (2) ===="
  let hpt = hsc_HPT hsc_env
      home_mod_infos = eltsUFM hpt
      pkg_deps :: [PackageId]
      pkg_deps  = concatMap (map fst . dep_pkgs . mi_deps . hm_iface) home_mod_infos
      linkables :: [Linkable]
      linkables = map (expectJust "link" . hm_linkable) home_mod_infos
  putStrLn $ unwords $ map (show . packageIdString) pkg_deps
  putStrLn "=== Linkables ==========="
  putStrLn $ unwords $ map (show . moduleNameString . moduleName . linkableModule) linkables
  -}
  -- we will just inject the current HPT intp the building. That should
  -- contain all required linkable objects.
  bs <- buildDynamicLib n dflags (hsc_HPT hsc_env) ds_expr (show ty)

  putStrLn "=== Contacting Runner ==="
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
    mDeps e  = uniqSetToList . mkUniqSet . catMaybes $ map (fmap moduleName . nameModule_maybe . idName) (e ^.. template)
    eDeps e  = uniqSetToList . mkUniqSet . catMaybes $ map (fmap modulePackageId . nameModule_maybe . idName) (e ^.. template)

{-
linkTh :: GhcjsSettings        -- settings (contains the base state)
       -> [FilePath]           -- extra js files
       -> DynFlags             -- dynamic flags
       -> [PackageId]
       -> HomePackageTable     -- what to link
       -> Maybe ByteString     -- current module or Nothing to get the initial code + rts
       -> IO Gen2.LinkResult
linkTh settings js_files dflags expr_pkgs hpt code = do
  let home_mod_infos = eltsUFM hpt
      pidMap    = pkgIdMap (pkgState dflags)
      pkg_deps :: [PackageId]
      pkg_deps  = concatMap (map fst . dep_pkgs . mi_deps . hm_iface) home_mod_infos
      linkables = map (expectJust "link".hm_linkable) home_mod_infos
      getOfiles (LM _ _ us) = map nameOfObject (filter isObject us)
      -- fixme include filename here?
      obj_files = maybe [] (\b -> Left ("<Template Haskell>", b) : map Right (concatMap getOfiles linkables)) code
      packageLibPaths :: PackageId -> [FilePath]
      packageLibPaths pkg = maybe [] libraryDirs (lookupPackage pidMap pkg)
      dflags' = dflags { ways = WayDebug : ways dflags }
  -- link all packages that TH depends on, error if not configured
  (th_deps_pkgs, mk_th_deps) <- Gen2.thDeps dflags'
  (rts_deps_pkgs, _) <- Gen2.rtsDeps dflags'
  expr_pkgs_deps <- packageDeps dflags expr_pkgs
  let addDep pkgs name
        | any (matchPackageName name) pkgs = pkgs
        | otherwise = lookupRequiredPackage dflags "to run Template Haskell" name : pkgs
      pkg_deps' = closeDeps dflags (L.foldl' addDep pkg_deps (th_deps_pkgs ++ rts_deps_pkgs) ++ expr_pkgs_deps)
      th_deps   = mk_th_deps pkg_deps'
      th_deps'  = T.pack $ (show . L.nub . L.sort . map Gen2.funPackage . S.toList $ th_deps) ++ show (ways dflags')
      deps      = map (\pkg -> (pkg, packageLibPaths pkg)) pkg_deps'
      is_root   = const True
      -- pkgs      = map packageConfigId . eltsUFM . pkgIdMap . pkgState $ dflags
      link      = Gen2.link' dflags' settings "template haskell" [] deps obj_files js_files is_root th_deps
  if isJust code
     then link
     else Gen2.getCached dflags' "template-haskell" th_deps' >>= \case
            Just c  -> return (runGet get $ BL.fromStrict c)
            Nothing -> do
              lr <- link
              Gen2.putCached dflags' "template-haskell" th_deps'
                              [topDir dflags </> "ghcjs_boot.completed"]
                              (BL.toStrict . runPut . put $ lr)
              return lr
-}
{-
-- this is a hack because we don't have a TcGblEnv. Fix before 7.10
packageDeps :: DynFlags -> [PackageId] -> IO [PackageId]
packageDeps dflags pkgs = do
  let allPkgIds = map packageConfigId . eltsUFM . pkgIdMap . pkgState $ dflags
  configs <- filter ((`elem`pkgs) . packageConfigId) <$> getPreloadPackagesAnd dflags (filter (`elem` allPkgIds) pkgs)
  let allDeps = L.nub . map (\(InstalledPackageId i) -> i) . concatMap depends $ configs
  return $ filter (\p -> any (L.isPrefixOf (packageIdString p)) allDeps || p `elem` pkgs) allPkgIds

-- get the closure the dependency graph
closeDeps :: DynFlags -> [PackageId] -> [PackageId]
closeDeps dflags pkgs = map packageConfigId $ go (map getInstalledPackage pkgs)
  where
    p       = pkgIdMap . pkgState $ dflags
    allPkgs = eltsUFM . pkgIdMap . pkgState $ dflags
    getInstalledPackage pkgId =
      fromMaybe (error ("cannot find package " ++ show pkgId)) (lookupPackage p pkgId)
    lookupInstalledPackage ipid =
      case filter ((==ipid) . installedPackageId) allPkgs of
        (x:_) -> x
        _     -> error $ "cannot find package id " ++ show ipid
    go :: [PackageConfig] -> [PackageConfig]
    go xs
      | length xs == length xs' = xs
      | otherwise               = go xs'
      where
        xs' = L.nubBy ((==) `on` installedPackageId) $
              concatMap (\x -> x : map lookupInstalledPackage (depends x)) xs

-}
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
