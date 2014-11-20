{-# LANGUAGE Rank2Types #-}
module ThCargo where
import Foreign.StablePtr
import Language.Haskell.TH.Lib ( ExpQ )
import Language.Haskell.TH.Syntax ( Quasi, Exp(LitE), Lit(StringL) {-Exp(ConE), mkName -})

foreign export ccall "getAction" getAction :: IO (StablePtr Action)

data Action = QuasiAction  (ExpQ)

getAction :: IO (StablePtr Action)
getAction = newStablePtr $ QuasiAction shippedSplice

shippedSplice :: ExpQ
shippedSplice = return $ LitE (StringL "Nothing")
