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
    dflags <- getDynFlags
    let dflags' = xopt_set dflags Opt_RankNTypes
    _ <- setSessionDynFlags dflags'
    (dflags'',_) <- liftIO $ initPackages dflags'
--    addTarget $ Target (TargetFile "./OOPTH/Data.hs" Nothing) True Nothing
    addTarget target
    -- load all targets
    _ <- load LoadAllTargets
    -- locate the dependencies.
    
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
          (output_fn', _) <- hscGenHardCode hsc_env cg_guts modSummary output_fn
          -- also see DriverPileline:285
          -- TODO: - finish the compilation and link steps.
          --       - adjust the hsc_env/dflags to produce a shared object.
          --       - send that sharedObejct to the runner.
          --       - read back the produced result.
          putStrLn output_fn'
          putStrLn "Done!"
        return ()
      Nothing -> panic "failed to locate module"
