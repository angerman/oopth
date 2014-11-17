{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module Main where
import Language.Haskell.TH.Syntax --(Quasi, Exp(ConE), mkName)
import Serialized
import Outputable
import Binary

main :: IO ()
main = do
  let dat = toSerialized serializeWithData exampleExp
  h   <- openBinMem (4096*2)
  _   <- put_ h dat
  _   <- writeBinMem h "quasi.th"
  putStrLn "wrote"

exampleExp :: Exp
exampleExp = InfixE (Just (LitE (IntegerL 1))) (VarE $ mkName "+") (Just (LitE (IntegerL 2))) --[|1+1|]--(mkName "Nothing")

exampleExpr :: Quasi m => m Exp
exampleExpr = return exampleExp
