module THTestLib where

import Foreign.StablePtr
import Language.Haskell.TH.Syntax (Quasi, Exp(ConE), mkName)

import OOPTH.Data

foreign export ccall getAction :: IO (StablePtr Action)

getAction :: IO (StablePtr Action)
getAction = newStablePtr $ QuasiAction exampleExpr

exampleExpr :: Quasi m => m Exp
exampleExpr = return (ConE (mkName "Nothing"))
