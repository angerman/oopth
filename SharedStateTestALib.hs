module SharedStateTestALib where

import GHC.IO.Encoding (getLocaleEncoding, setLocaleEncoding, latin1)

import Foreign.StablePtr

import OOPTH.Data

foreign export ccall getAction :: IO (StablePtr Action)

getAction :: IO (StablePtr Action)
getAction = newStablePtr $ IOAction testLocaleEncoding

testLocaleEncoding :: IO ()
testLocaleEncoding = do
  putStrLn "locale encoding library A, before/after:"
  print =<< getLocaleEncoding
  setLocaleEncoding latin1
  print =<< getLocaleEncoding
