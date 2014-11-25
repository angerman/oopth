module OutOfProcTH (installOOPTHHook) where

import OutOfProcTHBuilder ( buildDynamicLib )
import Control.Distributed.Process.Backend.SimpleLocalnet
import Com

-- Hooks
import DynFlags
import Hooks
import BasicTypes
import Language.Haskell.TH.Syntax
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
import Outputable

-- running the runner
import System.Process (createProcess, cwd, proc, waitForProcess)
import System.Exit (ExitCode(..))
import System.IO (hFlush, stdout)
-- reading the value from the file
import Binary
import Serialized

installOOPTHHook :: DynFlags -> DynFlags
installOOPTHHook dyflags = dyflags { hooks = addHooks (hooks dyflags) }
  where
    addHooks h = h { hscCompileCoreExprHook = Just compileCoreExpr }

myResult :: HValue
myResult = unsafeCoerce myExampleExpr

myExampleExpr :: Q Exp
myExampleExpr = return (ConE (mkName "Nothing"))

readValueFromFile :: FilePath -> IO (Q Exp)
readValueFromFile f = do
  h   <- readBinMem f
  dat <- get h
  let Just value = fromSerialized deserializeWithData dat :: Maybe Exp
  return . return $ value

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

-- most of this is an adapation from ghcjsCompileCoreExpr (Gen2.TH from ghcjs)
compileCoreExpr :: HscEnv -> SrcSpan -> CoreExpr -> IO HValue
compileCoreExpr hsc_env srcspan ds_expr = do
  -- see HscMain.hs:1653 for the default method.
  putStrLn "=== Compile Core Expr ==="
  buildDynamicLib dflags ds_expr

  putStrLn "=== Contacting Runner ==="
  backend <- initializeBackend "localhost" "8081" rtable
  result  <- runLocal backend thClientProc
  putStrLn $ show (unsafeCoerce result :: Exp)
--  result <- readHValueFromRunner "a.out"
--  putStrLn $ showPpr dflags ds_expr
--  prep_expr <- corePrepExpr dflags hsc_env ds_expr
--  putStrLn $ showSDoc dflags (pprCoreExpr prep_expr)
--  putStrLn "-------------------------"
--  let n = 0
--      bs = [bind n prep_expr]
--  putStrLn $ showPpr dflags bs
--  simpl_expr <- simplifyExpr dflags bs
--  let tidy_expr = tidyExpr emptyTidyEnv simpl_expr
--  putStrLn $ showPpr dflags tidy_expr
  putStrLn "========= End ==========="
--  putStrLn $ show ce
  -- TODO:
  --  1. Build rest of program around ce.
  --  2. Turn ce into a .dylib
  --  3. rund and execute the .dylib
  --  4. return the result.
  return result
  where
--    isNonQ   = show ty == "GHC.Desugar.AnnotationWrapper"
--    symb n   = "h$thrunnerZCThRunner" <> T.pack (show n) <> "zithExpr"
--    ty       = expandTypeSynonyms (exprType ds_expr)
--    thExpr n = mkVanillaGlobal (mkExternalName (mkRegSingleUnique (1+n)) (mod n) (mkVarOcc "thExpr") srcspan) ty
--    bind n e = NonRec (thExpr n) e
--    mod n    = mkModule pkg (mkModuleName $ "ThRunner" ++ show n)
--    pkg      = stringToPackageKey "thrunner"
    dflags   = hsc_dflags hsc_env
--    eDeps e  = uniqSetToList . mkUniqSet . catMaybes $ map (fmap modulePackageId . nameModule_maybe . idName) (e ^.. template)
