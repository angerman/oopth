module Main where

import Control.Distributed.Process.Backend.SimpleLocalnet
import System.Environment (getArgs)
import Com

import Unsafe.Coerce (unsafeCoerce)
import qualified Language.Haskell.TH        as TH
import qualified Language.Haskell.TH.Syntax as TH


main :: IO ()
main = do
  args <- getArgs

  case args of
    ["master", host, port] -> do
      backend <- initializeBackend host port rtable
      startMaster backend (master backend)
    ["test"] -> do
      backend <- initializeBackend "localhost" "8081" rtable
      putStrLn "Starting"
      resp <- runLocal backend thClientProc
      putStrLn $ show (unsafeCoerce resp :: TH.Exp)
      putStrLn "Done"
    ["slave", host, port] -> do
      backend <- initializeBackend host port rtable 
      startSlave backend
