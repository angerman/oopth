module Test.Str(str, exampleExpr) where
 
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
 
str = QuasiQuoter { quoteExp = stringE }

exampleExpr :: Quasi m => m Exp
exampleExpr = return (ConE (mkName "Nothing"))
