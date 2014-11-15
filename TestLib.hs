{-# LANGUAGE ForeignFunctionInterface #-}

module TestLib where

import Data.Char (toUpper)
import Foreign.StablePtr

data Action = Action (String -> IO String)

foreign export ccall getAction :: IO (StablePtr Action)

getAction :: IO (StablePtr Action)
getAction = newStablePtr $ Action test

test :: String -> IO String
test = return . map toUpper
