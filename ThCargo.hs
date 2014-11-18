{-# LANGUAGE Rank2Types #-}
module ThCargo where
import Foreign.StablePtr
import Language.Haskell.TH.Syntax ( Quasi, Exp(ConE), mkName )

foreign export ccall "getAction" getAction :: IO (StablePtr Action)

data Action = QuasiAction  (Quasi m => m Exp)

getAction :: IO (StablePtr Action)
getAction = newStablePtr $ QuasiAction (return shippedSplice)

shippedSplice :: Exp
shippedSplice = undefined
