module Main where

import Network.Endpoints
import Network.Transport.TCP
import Control.Concurrent (threadDelay)

import System.Environment (getArgs)

import Unsafe.Coerce (unsafeCoerce)
import qualified Language.Haskell.TH        as TH
import qualified Language.Haskell.TH.Syntax as TH


import OOPTH.Eval

main :: IO ()
main = do
  args <- getArgs

  let master = "master"
      slave  = "slave"
      resolver = resolverFromList [(master, "localhost:2001"),
                                   (slave,  "localhost:2000")]
  transport <- newTCPTransport resolver

  case args of
    ["master"] -> undefined
    ["test"] -> undefined
    ["slave"] -> do
      endpoint <- newEndpoint [transport]
      Right () <- bindEndpoint endpoint slave
      runTHServer (receiveMessage endpoint) (sendMessage_ endpoint master)
      threadDelay 1000
      unbindEndpoint endpoint slave
      shutdown transport

