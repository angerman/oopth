{-# LANGUAGE Rank2Types #-}
module OOPTH.Import where

import System.Posix.DynamicLinker
import System.IO (FilePath)
import Foreign.StablePtr
import Foreign.Ptr (FunPtr)
import Language.Haskell.TH.Syntax (Quasi, Exp)

import OOPTH.Data

type WrappedAction = IO (StablePtr Action)
foreign import ccall "dynamic" unwrap :: FunPtr WrappedAction -> WrappedAction

loadActionFromLibrary :: FilePath -> IO Action
loadActionFromLibrary file = do
  dl             <- dlopen file []
  wrappedAction  <- dlsym dl "getAction"
  action         <- unwrap  wrappedAction
  deRefStablePtr action

loadIOActionFromLibrary :: FilePath -> IO (IO ())
loadIOActionFromLibrary file = do
  IOAction a     <- loadActionFromLibrary file
  return a

loadStringActionFromLibrary :: Monad m => FilePath -> IO (String -> m String)
loadStringActionFromLibrary file = do
  StringAction a <- loadActionFromLibrary file
  return a

loadQuasiActionFromLibrary :: Quasi m => FilePath -> IO (m Exp)
loadQuasiActionFromLibrary file = do
  QuasiAction a  <- loadActionFromLibrary file
  return a

