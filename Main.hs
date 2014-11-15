{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import System.Posix.DynamicLinker
import Foreign.StablePtr
import Foreign.Ptr

import OOPTH.Data

type WrappedAction = IO (StablePtr Action)
foreign import ccall "dynamic" unwrapAction :: FunPtr WrappedAction -> WrappedAction

main :: IO ()
main = do
  withDL_ "./TestLib.dylib" [] $ \dl -> do
    wrappedAction   <- dlsym dl "getAction"
    action          <- unwrapAction wrappedAction
    StringAction fn <- deRefStablePtr action
    print           =<< fn "hello, world"
  -- 
  dl1 <- dlopen "./SharedStateTestALib.dylib" []
  sa1 <- getSimpleAction dl1
  sa1
  dl2 <- dlopen "./SharedStateTestBLib.dylib" []
  sa2 <- getSimpleAction dl2
  sa2
  -- We might want to close dl1 and dl2
  where
    getSimpleAction dl = do
      wrappedAction  <- dlsym dl "getAction"
      action         <- unwrapAction wrappedAction
      SimpleAction a <- deRefStablePtr action
      return a
  
