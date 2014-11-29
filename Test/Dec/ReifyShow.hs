{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

module Test.Dec.ReifyShow where

import Language.Haskell.TH

import Data.List (intercalate)

reifyShow :: Name -> Q [Dec]
reifyShow name = do
    TyConI (DataD _ _ _ [RecC _ fields] _) <- reify name
    let names = map (\(name,_,_) -> name) fields
    let showField :: Name -> Q Exp
        showField name = [|\x -> s ++ " = " ++ show ($(varE name) x)|] where
            s = nameBase name
    let showFields :: Q Exp
        showFields = listE $ map showField names
    [d|instance Show $(conT name) where
        show x = intercalate ", " (map ($ x) $showFields)|]
