{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import OOPTH.Data
import OOPTH.Import

main :: IO ()
main = do
  fn      <- loadStringActionFromLibrary "./TestLib.dylib"
  print  =<< fn "hello, world"
  --
  action1 <- loadIOActionFromLibrary "./SharedStateTestALib.dylib"
  action1
  action2 <- loadIOActionFromLibrary "./SharedStateTestBLib.dylib"
  action2
