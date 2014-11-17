module Main where

import System.IO (hFlush, stdout, FilePath)
import Control.Monad (forever)
import Language.Haskell.TH.Syntax (Quasi, Exp, runQ)
import Binary
import Serialized
import Data.Typeable (Typeable)
import Data.Data (Data)

import OOPTH.Data
import OOPTH.Import

import Language.Haskell.TH.Syntax (Quasi, Exp, runQ)

waitForNewLibraryFile :: IO FilePath
waitForNewLibraryFile = getLine

waitForNewDataFile :: IO FilePath
waitForNewDataFile = getLine

deserializeActionFromFile :: Quasi m => FilePath -> IO (m Exp)
deserializeActionFromFile file = do
  h    <- readBinMem file
  dat  <- get h
  let Just action = fromSerialized deserializeWithData dat
  return . return $ action

serializeResultToFile :: (Typeable a, Data a) => FilePath -> a -> IO ()
serializeResultToFile file result = do
  let dat = toSerialized serializeWithData result
  h      <- openBinMem (4096*2)
  _      <- put_ h dat
  writeBinMem h file

main :: IO ()
main = forever $ do
  putStrLn "Please enter the filepath to library with QuasiAction"
  putStr "OOPTH Runner> "
  hFlush stdout
  file   <- waitForNewLibraryFile
  action <- loadQuasiActionFromLibrary file
  result <- runQ action
  print result
  _      <- serializeResultToFile (file ++ "c") result
  putStrLn $ "wrote " ++ file ++ "c"
