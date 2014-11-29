{-# LANGUAGE TemplateHaskell #-}

import Test.Dec.CustomShow

data MyData = MyData
    { foo :: String
    , bar :: Int
    }

emptyShow ''MyData

main = print $ MyData { foo = "bar", bar = 5 }
