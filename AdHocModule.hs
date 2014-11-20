module Main where

import Control.Monad
import MonadUtils       ( liftIO )
import System.IO        ( hFlush, stdout )

import RdrHsSyn
import FastString
import Lexer
import ForeignCall
import OccName
import RdrName
import HscMain
import TcRnDriver
import HscTypes
import ErrUtils
import DriverPipeline
import DriverPhases
import PipelineMonad
import Util
import UniqFM

import HsSyn
import CoreSyn
import SrcLoc
import Module
import Debug.Trace
import PprCore
import Outputable
import DynFlags

import GHC
import qualified GHC.Paths
import Packages

import Data.List ( find )

{-
The goal is to use the ThCargo.hs file as a base, adjust the
module name into something temporary, and replace the `undefined`
node with the acutal th splice.  (e.g. obtained from the
`hscCompileCoreExpr` (HscMain.hs) though the `hscCompileCoreExprHook`.

Or though the (new) `runMeta` hook. (See preliminary patch: http://lpaste.net/114451)

Note: Alternatively one could construct the module by hand as well.

run with

$ cabal run oopth-adhoc-module 
-}

main :: IO ()
main = do
  let fn = "ThCargo.hs"
  let target = Target (TargetFile fn Nothing) True Nothing -- The last one could be Just (StringBuffer, UTCTime) for in-mem.
  runGhc (Just GHC.Paths.libdir) $ do
    dflags_raw <- getDynFlags
    let dflags = (xopt_set dflags_raw Opt_RankNTypes)
    _ <- setSessionDynFlags dflags
    (dflags_raw',_) <- liftIO $ initPackages dflags
--    addTarget $ Target (TargetFile "./OOPTH/Data.hs" Nothing) True Nothing
    addTarget target
    -- load all targets
    _ <- load LoadAllTargets
    -- locate the dependencies.

    (dflags_,lo,ws) <- parseDynamicFlagsCmdLine dflags_raw' [noLoc "-no-hs-main", noLoc "-dynamic", noLoc "-shared"]
    let dflags'' = dflags_ { verbosity = 1 }
    
    modGraph <- depanal [] True
    case find ((== fn) . msHsFilePath) modGraph of
      Just modSummary -> do
        liftIO $ putStrLn "Found Module"
        liftIO $ hFlush stdout
        liftIO $ do
          hsc_env <- newHscEnv dflags''
          -- hscParse :: HscEnv -> ModSummary -> IO HsParsedModule
          mod     <- hscParse hsc_env modSummary
          putStrLn $ showSDoc dflags'' (ppr $ (unLoc . hpm_module $ mod))
          -- hscTypecheckRename :: HscEnv -> ModSummary -> HsParsedModule -> IO (TcGblEnv, RenamedStuff)
          (tcGblEnv, rnStuff) <- hscTypecheckRename hsc_env modSummary mod
          -- hscDesugar :: HscEnv -> ModSummary -> TcGblEnv -> IO ModGuts
          modguts <- hscDesugar hsc_env modSummary tcGblEnv
          -- hscNormalIface :: HscEnv -> {- FilePath -> -} ModGuts -> Maybe Fingerprint -> IO (ModIface, Bool, ModDetails, CgGuts)
          (new_iface, no_change, details, cg_guts) <- hscNormalIface hsc_env "ThCargo.ec" modguts Nothing
          let output_fn = "ThCargo"
-- runPipeline is not avaialbe from outside :(
--          (dflags''', output_fn') <- runPipeline StopLn hsc_env
--                                    (output_fn, Just (HscOut HsSrcFile (ms_mod_name modSummary) (HscRecomp cg_guts modSummary)))
--                                    (Just "ThCargo")
--                                    Persistent
--                                    (Just (ms_location modSummary))
--                                    Nothing
          -- hscGenHardCode :: HscEnv -> GgGuts -> ModSummary -> FilePath -> IO (FilePath, Maybe FilePath)
          (output_fn', stub) <- hscGenHardCode hsc_env cg_guts modSummary output_fn
          putStrLn $ show stub

          -- figure out what's the next phase
          -- hscPostBackendPhase :: DynFlags -> HscSource -> HscTarget -> Phase
          let phase = hscPostBackendPhase dflags'' HsSrcFile (hscTarget dflags'')
          putStrLn $ "Phase: " ++ (show phase)

          -- build up the pipleline env and state
          let env = PipeEnv{ pe_isHaskellishFile = False
                           , stop_phase   = StopLn
                           , src_filename = output_fn'
                           , src_basename = output_fn
                           , src_suffix   =  ""
                           , output_spec  = Persistent }
              state = PipeState{ hsc_env   = hsc_env
                               , maybe_loc = Just (ModLocation (Just "ThCargo.hs") "ThCargo.hi" "ThCargo.o")
                               , maybe_stub_o = Nothing }
          -- run the phase.
          (phase, object_filename) <- evalP (runPhase (RealPhase phase) output_fn' dflags'')  env state
          -- we expect this to be (StopLn, "ThCargo.o")
          
          putStrLn $ "O.: " ++ object_filename

          -- Try to link.
          -- let's try to build up a HomePackageTable
          o_time <- getModificationUTCTime object_filename
          let this_mod = ms_mod modSummary
              linkable = LM o_time this_mod [DotO object_filename, DotO "ffi_stub.x86_64o"]
              hmi = HomeModInfo{ hm_iface = new_iface
                               , hm_details = details
                               , hm_linkable = Just linkable
                               }
              hpt = unitUFM this_mod hmi
              
          -- link :: GhcLink {- interactive or batch -} -> DynFlags {- dynamic flags -} -> Bool {- attempt linking in batch mode? -} -> HomePackageTable {- what to link -} -> IO SuccessFlag
          -- 
          linked <- link (ghcLink dflags'') dflags'' True hpt
          case linked of
            Succeeded -> putStrLn "Linked!"
            Failed    -> putStrLn "Failed to link!"

          -- FIXME:
          -- This whole process so far only generates ONE shared object.
          -- But, we are missing the important ffi parts.
          -- This is done though a c file that is turned into a asm file
          -- which in turn is turned into an object, and that object is
          -- linked with the produced binary.
          -- We just use a known interface to link.  Which is pretty stupid!

            
          -- also see DriverPileline:285
          -- TODO: - finish the compilation and link steps.
          --       - send that sharedObejct to the runner.
          --       - read back the produced result.
          putStrLn output_fn'
          putStrLn "Done!"
        return ()
      Nothing -> panic "failed to locate module"
