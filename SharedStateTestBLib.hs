module SharedStateTestBLib where

import GHC.IO.Encoding (getLocaleEncoding, setLocaleEncoding, utf8)

import Foreign.StablePtr

import OOPTH.Data

foreign export ccall getAction :: IO (StablePtr Action)

getAction :: IO (StablePtr Action)
getAction = newStablePtr $ SimpleAction testLocaleEncoding

testLocaleEncoding :: IO ()
testLocaleEncoding = do
  putStrLn "locale encoding library B, before/after:"
  print =<< getLocaleEncoding
  setLocaleEncoding utf8
  print =<< getLocaleEncoding
