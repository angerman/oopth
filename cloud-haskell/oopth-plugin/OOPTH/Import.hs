{-# LANGUAGE Rank2Types #-}
module OOPTH.Import where

import System.Posix.DynamicLinker
import System.IO (FilePath)
import Foreign.StablePtr
import Foreign.Ptr (FunPtr)
import Language.Haskell.TH.Syntax (Quasi, Exp)

import GHC.Prim (Any)

data Action = QuasiAction Any -- (This will be Quasi m => m Exp, or a like)


type WrappedAction = IO (StablePtr Action)
foreign import ccall "dynamic" unwrap :: FunPtr WrappedAction -> WrappedAction

loadActionFromLibrary :: FilePath -> IO Action
loadActionFromLibrary file = do
  putStrLn "Loading dylib..."
  dl             <- dlopen file []
  putStrLn "Looking up symbol..."
  wrappedAction  <- dlsym dl "getAction"
  putStrLn . show $ wrappedAction
  putStrLn "Unwrapping action..."
  action         <- unwrap  wrappedAction
  putStrLn "Dereferencing stable ptr..."
  deRefStablePtr action

{-
loadIOActionFromLibrary :: FilePath -> IO (IO ())
loadIOActionFromLibrary file = do
  IOAction a     <- loadActionFromLibrary file
  return a

loadStringActionFromLibrary :: Monad m => FilePath -> IO (String -> m String)
loadStringActionFromLibrary file = do
  StringAction a <- loadActionFromLibrary file
  return a
-}
loadQuasiActionFromLibrary :: FilePath -> IO Any
loadQuasiActionFromLibrary file = do
  QuasiAction a  <- loadActionFromLibrary file
  return a

