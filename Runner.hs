{-# LANGUAGE Rank2Types #-}
module Main where

import System.Posix.DynamicLinker
import System.IO (FilePath)
import Control.Monad (forever)
import Foreign.StablePtr
import Foreign.Ptr

import OOPTH.Data

import Language.Haskell.TH.Syntax (Quasi, Exp, runQ)


type WrappedAction = IO (StablePtr Action)
foreign import ccall "dynamic" unwrapAction :: FunPtr WrappedAction -> WrappedAction

waitForNewLibraryFile :: IO FilePath
waitForNewLibraryFile = getLine

getTemplateHaskellAction :: FilePath -> Quasi m => IO (m Exp)
getTemplateHaskellAction file = do
  dl             <- dlopen file []
  wrappedAction  <- dlsym dl "getAction"
  action         <- unwrapAction wrappedAction
  QuasiAction a  <- deRefStablePtr action
  return a

main :: IO ()
main = forever $ do
  file   <- waitForNewLibraryFile
  action <- getTemplateHaskellAction file
  print =<< runQ action
