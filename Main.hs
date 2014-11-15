{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import System.Posix.DynamicLinker
import Foreign.StablePtr
import Foreign.Ptr

data Action = Action (String -> IO String)

type WrappedAction = IO (StablePtr Action)
foreign import ccall "dynamic" unwrapAction :: FunPtr WrappedAction -> WrappedAction

main :: IO ()
main = withDL_ "./TestLib.dylib" [] $ \dl -> do
  wrappedAction <- dlsym dl "getAction"
  action        <- unwrapAction wrappedAction
  Action fn     <- deRefStablePtr action
  print         =<< fn "hello, world"
