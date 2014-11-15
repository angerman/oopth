{-# LANGUAGE ForeignFunctionInterface #-}

module TestLib where

import Data.Char (toUpper)
import Foreign.StablePtr

import OOPTH.Data

foreign export ccall getAction :: IO (StablePtr Action)

getAction :: IO (StablePtr Action)
getAction = newStablePtr $ StringAction test

test :: String -> IO String
test = return . map toUpper
