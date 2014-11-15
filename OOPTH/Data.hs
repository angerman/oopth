{-# LANGUAGE Rank2Types #-}
module OOPTH.Data where

import Language.Haskell.TH.Syntax (Quasi, Exp)

data Action = SimpleAction (IO ())
            | StringAction (String -> IO String)
            | TemplHAction (Quasi m => m Exp)
