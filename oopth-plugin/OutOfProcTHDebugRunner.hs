{-# LANGUAGE Rank2Types #-}
module Main where

import System.IO (hFlush, stdout, FilePath)
import Control.Monad (forever)

import OOPTH.Data
import OOPTH.Import

import Language.Haskell.TH.Syntax (Quasi, Exp, runQ, Q)
import           Unsafe.Coerce

waitForNewLibraryFile :: IO FilePath
waitForNewLibraryFile = getLine

main :: IO ()
main = forever $ do
  putStrLn "Please enter the filepath to library with QuasiAction"
  putStr "OOPTH Runner> "
  hFlush stdout
  file   <- waitForNewLibraryFile
  action <- loadQuasiActionFromLibrary file
  print =<< runQ (unsafeCoerce action :: Q Exp)
