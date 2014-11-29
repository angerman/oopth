{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

module Test.Dec.CustomShow where

import Language.Haskell.TH

emptyShow :: Name -> Q [Dec]
emptyShow name = [d|instance Show $(conT name) where show _ = "Empty show"|]
