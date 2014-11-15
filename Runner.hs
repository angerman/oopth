{-# LANGUAGE Rank2Types #-}
module Main where

import System.IO (FilePath)
import Control.Monad (forever)

import OOPTH.Data
import OOPTH.Import

import Language.Haskell.TH.Syntax (Quasi, Exp, runQ)

waitForNewLibraryFile :: IO FilePath
waitForNewLibraryFile = getLine

main :: IO ()
main = forever $ do
  file   <- waitForNewLibraryFile
  action <- loadQuasiActionFromLibrary file
  print =<< runQ action
