module Main where

import Control.Monad
import MonadUtils       ( liftIO )

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

import HsSyn
import SrcLoc
import Module
import Debug.Trace
import PprCore
import Outputable
import DynFlags

import GHC
import qualified GHC.Paths
import Packages

{-
We try to build a module similar to

module ThCargo where
import Foreign.StablePtr
import Language.Haskell.TH.Syntax ( Quasi, Exp(ConE), mkName )
import OOPTH.Data

foreign export ccall "getAction" getAction :: IO (StablePtr Action)

getAction :: IO (StablePtr Action)
getAction = newStablePtr $ QuasiAction exampleExpr

exampleExpr :: Quasi m => m Exp
exampleExpr = return (ConE (mkName "Nothing"))
-}

main :: IO ()
main = do
  runGhc (Just GHC.Paths.libdir) $ do
    dflags <- getDynFlags
    (dflags,_) <- liftIO $ initPackages dflags
    _ <- setSessionDynFlags dflags
    _ <- load (LoadUpTo $ mkModuleName "OOPTH.Data")

    let mod = mkAdHocModule
    liftIO $ do
      putStrLn ".=== Module ==="
      putStrLn $ showSDoc dflags (ppr mod)
      putStrLn "'--------------"

    let parsedMod = HsParsedModule (noLoc mod) []
    env <- liftIO $ newHscEnv dflags
    ((_,msgs), globalMod) <- liftIO $ tcRnModule env HsSrcFile True parsedMod
    liftIO $ printBagOfErrors dflags msgs

mkAdHocModule :: HsModule RdrName
mkAdHocModule = HsModule (Just (noLoc . mkModuleName $ "ThCargo")) Nothing imports decls Nothing Nothing
  where
    imports = map (noLoc . simpleImportDecl . mkModuleName) ["Foreign.StablePtr"
                                                            ,"Language.Haskell.TH.Syntax"
                                                            ,"OOPTH.Data"]
    decls   = [noLoc mkExportDecl]

mkExportDecl :: HsDecl RdrName
mkExportDecl = ForD (ForeignExport (noLoc v) (noLoc ty)
                     noForeignExportCoercionYet
                     (CExport (CExportStatic (mkFastString "getAction") CCallConv)))
    where
      v    = mkVarUnqual . mkFastString $ "getAction"
      -- Build type: IO (StablePtr Action)
      ty   = HsAppTy (mkTV "IO") (noLoc $ HsAppTy (mkTV "StablePtr") (mkTV "Action"))
      -- creates an unqualified typevariable with no location from a String.
      mkTV = noLoc . HsTyVar . mkVarUnqual . mkFastString
