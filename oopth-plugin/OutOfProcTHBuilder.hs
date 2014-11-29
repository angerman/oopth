{-# LANGUAGE CPP #-}
module OutOfProcTHBuilder (buildDynamicLib) where

import MonadUtils     ( liftIO )
import DynFlags
import HscTypes
import HscMain        ( hscCompileOneShot, newHscEnv )
import DriverPipeline ( compileOne, link, runPhase, PhasePlus, CompPipeline )
import PipelineMonad  ( PhasePlus( HscOut ) )
import Outputable     ( panic )
import GHC
import Packages
import Data.List      ( find )
import UniqFM         ( unitUFM, addToUFM )
import Hooks          ( hscCompileOneShotHook, runPhaseHook )
import Outputable     ( showSDoc, ppr )

import CoreSyn        ( CoreProgram, Bind( NonRec ), CoreExpr, Expr(Lit, App) {- test expr -} )
import Literal        ( mkMachString ) -- test expr
import Var            ( varName, mkGlobalVar )
import Name           ( nameOccName )
import OccName        ( OccName, mkVarOcc )
import Id             ( mkVanillaGlobal )
import StringBuffer   ( stringToStringBuffer )
import Data.Time.Clock( getCurrentTime )
import SysTools
import Data.ByteString( ByteString, readFile )


--------------------------------------------------------------------------------
-- getProcessID
-- stolen from SysTools (because getProcessID is not exported :-/)
#ifndef mingw32_HOST_OS
import qualified System.Posix.Internals
#else /* Must be Win32 */
import Foreign
import Foreign.C.String
#endif

#ifdef mingw32_HOST_OS
foreign import ccall unsafe "_getpid" getProcessID :: IO Int -- relies on Int == Int32 on Windows
#else
getProcessID :: IO Int
getProcessID = System.Posix.Internals.c_getpid >>= return . fromIntegral
#endif
--------------------------------------------------------------------------------

-- libdir :: FilePath
-- libdir  = GHC_PATHS_LIBDIR

injectSplice :: DynFlags -> CoreExpr -> CoreProgram -> IO CoreProgram
injectSplice df expr = go 0
  where
    lookupOccName :: OccName
    lookupOccName = mkVarOcc "shippedSplice"
    go :: Int -> CoreProgram -> IO CoreProgram
    go n (x:xs) = do
      -- show where we are.
--      putStrLn $ (show n) ++ ": " ++ (showSDoc df . ppr) x
      -- show expr.
      x' <- case x of
        NonRec v v2 ->
          if (nameOccName . varName $ v) == lookupOccName then
             do
               putStrLn "FOUND IT!"
               putStrLn $ (show n) ++ "==" ++ ((showSDoc df . ppr) $ nameOccName . varName $ v)
               return $ NonRec v expr
          else
            return x
        otherwise -> return x

      -- recurse
      xs' <- case xs of
        [] -> return []
        otherwise -> go (n+1) xs
      return (x':xs')

myRunPhaseHook :: CoreExpr -> PhasePlus -> FilePath -> DynFlags -> CompPipeline (PhasePlus, FilePath)
myRunPhaseHook expr rp fp df = do
  rp' <- case rp of
    (HscOut hscSource moduleName hscStatus) ->
      case hscStatus of
        HscRecomp guts modSummary -> do
          let program  = cg_binds guts
          injected     <- liftIO $ injectSplice df expr program
          let guts'    = guts { cg_binds = injected }
          liftIO $ putStrLn . (showSDoc df) . ppr $ program
          liftIO $ putStrLn "==== After Injection ===="
          liftIO $ putStrLn . (showSDoc df) . ppr $ injected
          return $ HscOut hscSource moduleName (HscRecomp guts' modSummary)
        otherwise -> panic "This was not expected"
    otherwise -> return rp
  (result, filePath) <- runPhase rp' fp df
  liftIO $ putStrLn $ "Phase " ++ (showSDoc df . ppr) rp ++ " -> " ++ (showSDoc df . ppr) result
  return (result, filePath)

buildDynamicLib :: Int -> DynFlags -> HomePackageTable -> CoreExpr -> IO ByteString
buildDynamicLib n master_dflags depHPT expr = do

  -- start a ghc session
  runGhc (Just $ topDir master_dflags) $ do
    dflags_ <- getDynFlags
    -- adjust the dynflags to contain RankNTypes.
    let dflags = (dflags_ `xopt_set` Opt_RankNTypes) { verbosity = verbosity master_dflags }
    -- configure the dynflags by parsing -no-hs-main, -dynamic and -shared.
    -- -dcore-lint will throw us off, as soon as our core turns up invalid!
    (dflags', _, _) <- parseDynamicFlagsCmdLine dflags $ map noLoc (words "-no-hs-main -dynamic -shared -dcore-lint")

    -- finally set the flags to the session and initialize and load the
    -- package set.
    setSessionDynFlags dflags'
    (dflags'',_) <- liftIO $ initPackages dflags'

    -- don't load targets.
    -- we haven't set any, and this will just create an empty a.out :-/
    -- load LoadAllTargets

    -- set up our container.
    (tmpFile, now) <- liftIO $ do
      tmpFile <- newTempName dflags "hs"
      putStrLn $ "Tempfile: " ++ tmpFile
      touch dflags"File must exists, to be found" tmpFile
      now <- getCurrentTime
      return (tmpFile, now)
    ghcProcId <- liftIO getProcessID
    let --file = "ThRunner.hs"
      moduleName = "GHC" ++ (show ghcProcId) ++ "ThRunner" ++ (show n)
      modName = mkModuleName moduleName
      targetId = --TargetModule modName --
                 TargetFile tmpFile Nothing
      targetBody = unlines ["{-# LANGUAGE Rank2Types #-}"
                           ,"module " ++ moduleName ++ " where"
                           ,"import Foreign.StablePtr"
                           ,"import Language.Haskell.TH.Lib ( ExpQ )"
                           ,"import Language.Haskell.TH.Syntax ( Quasi, Exp(LitE), Lit(StringL) )"

                           ,"foreign export ccall \"getAction\" getAction :: IO (StablePtr Action)"

                           ,"data Action = QuasiAction  (ExpQ)"

                           ,"getAction :: IO (StablePtr Action)"
                           ,"getAction = newStablePtr $ QuasiAction shippedSplice"

                           ,"shippedSplice :: ExpQ"
                           ,"shippedSplice = return $ LitE (StringL \"Nothing\")"]

      targetContents = Just (stringToStringBuffer targetBody, now)
      target = --Target  True Nothing
        Target targetId True targetContents

    -- add the new file as a target. If we had done this
    -- prior to load, we'd get compiled already.

    addTarget target
    -- find the module in the odule graph, and compile and link it.
    liftIO $ putStrLn "Running dependency analysis..."
    modGraph <- depanal [] True
    liftIO $ putStrLn "Ran dependency analysis..."    
    case find ((== modName) . ms_mod_name) modGraph of
      Just modSummary -> do
        liftIO $ do
          putStrLn "found module"
          -- install hook, but preserve the dflags
          -- the compileOne function will use the dflags from the moduleSummary, and superimpose them
          -- onto the hsc_env.  Therefore we install the hooks in the module, which's compilation we
          -- want to influence.
          let oldModDflags     = ms_hspp_opts modSummary
              new_hooks        = (hooks oldModDflags) { runPhaseHook = Just $ myRunPhaseHook expr }
              hookedModSummary = modSummary { ms_hspp_opts = oldModDflags { hooks = new_hooks } }
          let 
          hsc_env <- newHscEnv dflags''
--          state   <- hscCompileOneShot hsc_env "ThCargo.ec" modSummary SourceModified
--          case state of
--            HscRecomp _ _ -> putStrLn "Yey"
--            otherwise     -> putStrLn "Nay"
          putStrLn "Will Compile!"
          hmi <- compileOne hsc_env hookedModSummary 1 1 Nothing Nothing SourceModified
          putStrLn "Did Compile!"

          productpath <- newTempName dflags'' ".so"
          
          linked <- link (ghcLink dflags'') (dflags'' { outputFile = Just productpath }) True $
                    addToUFM depHPT (ms_mod modSummary) hmi
          case linked of
            Succeeded -> putStrLn "Linked!"
            Failed    -> putStrLn "Failed to link!"
          lib <- Data.ByteString.readFile productpath
          cleanTempFilesExcept dflags []
          return lib
      Nothing -> panic "failed to locate module"
