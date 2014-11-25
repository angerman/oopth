{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
-- from: https://www.fpcomplete.com/user/marcin/template-haskell-101
module Main where
import Language.Haskell.TH.Syntax (Quasi, Exp(ConE), mkName)
--import Test.Str

main :: IO ()
main = do
  let e = $([|Just "Out-Of-Proc-TH Splice!"|]) :: Maybe String --longString
  putStrLn . show $ e
  putStrLn "foo"

{-
longString =  [str|This is a multiline string.
It's many lines long.

 
 
It contains embedded newlines. And Unicode:
 
Ἐν ἀρχῇ ἦν ὁ Λόγος
 
It ends here: |]

-}
