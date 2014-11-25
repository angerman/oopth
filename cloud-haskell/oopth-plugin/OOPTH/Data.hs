{-# LANGUAGE Rank2Types #-}
module OOPTH.Data where

import Language.Haskell.TH.Syntax (Quasi, Exp)

data Action = IOAction     (IO ())
            | StringAction (Monad m => String -> m String)
            | QuasiAction  (Quasi m => m Exp)

