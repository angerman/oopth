{-# LANGUAGE QuasiQuotes #-}
-- from: https://www.fpcomplete.com/user/marcin/template-haskell-101

module Main where
import Test.QQ.Str
 
longString = [str|This is a multiline string.
It's many lines long.
 
 
It contains embedded newlines. And Unicode:
 
Ἐν ἀρχῇ ἦν ὁ Λόγος
 
It ends here: |]
 
main = putStrLn longString
