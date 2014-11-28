module OOPTH.Hacks where

import           Data.Char     (isSpace)
import           DynFlags
import           Type
import           Outputable    hiding ((<>))

-- this is a hack to be able to use pprShow in a Show instance, should be removed
{-# NOINLINE hackPprDflags #-}
hackPprDflags :: DynFlags
hackPprDflags = unsafeGlobalDynFlags

-- | replace all whitespace with space
fixSpace :: String -> String
fixSpace xs = map f xs
  where
    f c | isSpace c = ' '
        | otherwise = c
                      
instance Show Type where
  show ty = fixSpace (showPpr hackPprDflags ty)
