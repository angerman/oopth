module Main where


import           Network.Simple.TCP
import           Data.ByteString.Char8   ( pack, unpack )
import           System.Environment      ( getProgName, getArgs )
import           Control.Monad           ( unless )

import           Control.Concurrent      ( threadDelay )

import qualified Language.Haskell.TH        as TH
import qualified Language.Haskell.TH.Syntax as TH

import Network.Service
import Network.Transport.Encoding.Base64 ( mkService )



import OOPTH.Types
import OOPTH.Eval

mkServiceServer :: ServiceMessage a => ServiceName -> ServiceHandler a -> IO ()
mkServiceServer port shandler = serve HostAny port handler
  where handler :: (Socket, SockAddr) -> IO ()
        handler x = mkService x >>= shandler

mkServiceClient :: ServiceMessage a => HostName -> ServiceName -> IO (Service a)
mkServiceClient h p = connect h p mkService

main :: IO ()
main = do
  args <- getArgs
  prog <- getProgName

  case args of
    ["master"] -> undefined
    ["test", host, port] -> do
      service <- (mkServiceClient host port :: IO (Service Message))
      sTerm service
    ["slave"] -> mkServiceServer "2000" handler
    _  -> putStrLn $ "Usage: " ++ prog ++ " (server|client)"

  where handler :: ServiceHandler Message
        handler service = runTHServer (sRecv service) (sSend service)


-- TODO:
-- - Have a spawner slave, that can spawn new runner.
-- - if the spwaner cannot be found, log message and
--   retry
