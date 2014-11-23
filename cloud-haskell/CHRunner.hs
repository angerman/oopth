module Main where

import System.Environment (getArgs)
import Control.Distributed.Process
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Static (staticLabel, registerStatic, staticCompose)
import Data.Binary (encode, decode)
import Data.Rank1Dynamic (toDynamic)
import qualified Data.ByteString as B (ByteString, readFile, writeFile)
import Data.ByteString.Lazy as BL (ByteString, toStrict)

-- runner (loading, and runQing)
import OOPTH.Data
import OOPTH.Import

import Types
import Eval

import           Data.IORef

{-
instance Binary Serialized where
  put s = do
    IO bs  <- return $ do
      h  <- GHCB.openBinMem (4096*2) -- 8 MegaByte
      _  <- GHCB.put_ h s
      GHCB.get h :: (IO B.ByteString)
    put bs
  get = do
    bs <- get :: (Get B.ByteString)
    s <- liftIO $ do
      h <- GHCB.openBinMem (4096*2)
      _ <- GHCB.put_ h bs
      GHCB.get h :: (IO Serialized)
    return s
-}
--------------------------------------------------------------------------------
-- Master
{-
master :: Integer -> [NodeId] -> Process Integer
master n slaves = do
  us <- getSelfPid

  -- Start slave processes
  slaveProcesses <- forM slaves $
    \nid -> spawn nid ($(mkClosure 'slave) us)

  -- Distribute 1 .. n amongst the slave processes
  spawnLocal $ forM_ (zip [1 .. n] (cycle slaveProcesses)) $
    \(m, them) -> send them m

  -- Wait for the result
  sumIntegers (fromIntegral n)
-}

master :: Backend -> [NodeId] -> Process ()
master backend slaves = do
  us <- getSelfPid
  -- Do something interesting with the slaves
  log $ "[Client] Slaves: " ++ show slaves
  let firstSlave = head slaves
  pid <- spawn firstSlave (runTHServerClosure us)
  log "[Client] Reading file..."
  payload <- liftIO $ B.readFile "a.out"
  log "[Client] Sending Message..."
  send pid $ RunTH THExp payload Nothing
  log "[Client] Awaiting response..."
  msg <- expect :: Process Types.Message
  case msg of
    RunTH' bsr -> log $ "[Client] Received: " ++ (show bsr)
    _ -> log $ "[Client] Received: " ++ (show msg)
  -- Terminate the slaves when the master terminates (this is optional)
  terminateAllSlaves backend
  where
    log = liftIO . putStrLn

-- cloud-haskell-boilerplate
decodeProcessIdStatic :: Static (ByteString -> ProcessId)
decodeProcessIdStatic = staticLabel "$decodeProcessId"

runTHServerStatic :: Static (ProcessId -> Process ())
runTHServerStatic = staticLabel "$runTHServer"

runTHServerClosure :: ProcessId -> Closure (Process ())
runTHServerClosure pid = closure decoder (encode pid)
  where decoder :: Static (ByteString -> Process ())
        decoder = runTHServerStatic `staticCompose` decodeProcessIdStatic

--------------------------------------------------------------------------------
-- Slave
{-
slave :: ProcessId -> Process ()
slave them = forever $ do
  n <- expect
  send them (numPrimeFactors n)

remotable ['slave]
-}
--------------------------------------------------------------------------------
-- main
main :: IO ()
main = do
  args <- getArgs

  case args of
    ["master", host, port] -> do
      backend <- initializeBackend host port rtable 
      startMaster backend (master backend)
    ["slave", host, port] -> do
      backend <- initializeBackend host port rtable 
      startSlave backend
  where
    rtable = registerStatic "$runTHServer" (toDynamic runTHServer)
           . registerStatic "$decodeProcessId" (toDynamic (decode :: ByteString -> ProcessId))
           $ initRemoteTable
