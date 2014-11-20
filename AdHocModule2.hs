module Main where

import OutOfProcTHBuilder ( buildDynamicLib )

import CoreSyn        ( Expr(Lit, App) {- test expr -} )
import Literal        ( mkMachString ) -- test expr

main :: IO ()
main = do
  let expr = Lit (mkMachString "I'm in your core!")
  buildDynamicLib expr
