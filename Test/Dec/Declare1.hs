{-# LANGUAGE TemplateHaskell #-}
module Main where
import Language.Haskell.TH
import Test.Dec.Build1

$(build_p1)

main = print $ p1 (1,2)
