{-# LANGUAGE TemplateHaskell #-}
module Main where
import Language.Haskell.TH.Syntax (Quasi, Exp(ConE), mkName)

main :: IO ()
main = do
  let e = $([|Just "Out-Of-Proc-TH Splice!"|]) :: Maybe String
  putStrLn . show $ e
