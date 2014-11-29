{-# LANGUAGE TemplateHaskell #-}

import Test.Dec.ReifyShow

data MyData = MyData
    { foo :: String
    , bar :: Int
    }

reifyShow ''MyData

main = print $ MyData { foo = "bar", bar = 5 }
