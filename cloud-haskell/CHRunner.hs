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

import Language.Haskell.TH.Syntax as TH (Q(..), Quasi, Exp, runQ)
import Language.Haskell.TH as TH
import Data.Typeable (Typeable)
import Data.Data (Data)
import Serialized (Serialized, toSerialized, serializeWithData, fromSerialized, deserializeWithData)
import qualified Binary as GHCB
import Data.Binary (Binary, Get, get, put)
import Data.Binary.Put (runPut)

import Control.Applicative ((<$>))

anyToByteString :: (Typeable a, Data a) => a -> IO B.ByteString
anyToByteString e = do
  let dat = toSerialized serializeWithData e
  h  <- GHCB.openBinMem (4096*2) -- 8 MegaByte
  _  <- GHCB.put_ h dat
  GHCB.get h

byteStringToAny :: (Typeable a, Data a) => B.ByteString -> IO a
byteStringToAny bs = do
  h <- GHCB.openBinMem (4096*2)
  _ <- GHCB.put_ h bs
  s <- GHCB.get h :: IO Serialized
  let Just ret = fromSerialized deserializeWithData s
  return ret

runTHCode :: Binary a => TH.Q a -> TH.Q B.ByteString
runTHCode c = BL.toStrict . runPut . put <$> TH.runQ c
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
  liftIO . putStrLn $ "Slaves: " ++ show slaves
  let firstSlave = head slaves
  pid <- spawn firstSlave (reverseClosure us)
  payload <- liftIO $ B.readFile "a.out"
  send pid payload
  dat <- (expect :: Process B.ByteString) -- >>= return . return :: (Process (IO Exp))
  resp <- liftIO $ (byteStringToAny dat :: IO Exp)
--  let Just resp = fromSerialized deserializeWithData dat :: Maybe Exp
  liftIO . putStrLn $ "Received: " ++ (show resp)
  -- Terminate the slaves when the master terminates (this is optional)
  terminateAllSlaves backend

sendReverse :: ProcessId -> Process ()
sendReverse them = do
  s <- expect :: Process String
  send them $ reverse s

runQAction :: ProcessId -> Process ()
runQAction them = go 0
  where go :: Int -> Process ()
        go n = do
          log $ "[1 of 5] waiting for lib" ++ (show n)
          payload <- expect :: Process B.ByteString
          log $ "[2 of 5] received lib" ++ (show n)
          liftIO $ B.writeFile (lib n) payload
          log $ "[3 of 5] wrote lib" ++ (show n)
          action <- liftIO $ loadQuasiActionFromLibrary (lib n)
          log $ "[4 of 5] running action for lib" ++ (show n)
          Q result <- return $ (runQ action :: Q Exp)
          resultBS <- liftIO $ anyToByteString (result :: Exp)
          log $ "[5 of 5] returing result for lib" ++ (show n)
          send them resultBS
          go (n+1)
        log = liftIO . putStrLn
        lib n = "lib" ++ (show n) ++ ".dylib"

-- cloud-haskell-boilerplate
decodeProcessIdStatic :: Static (ByteString -> ProcessId)
decodeProcessIdStatic = staticLabel "$decodeProcessId"

runQActionStatic :: Static (ProcessId -> Process ())
runQActionStatic = staticLabel "$runQAction"

reverseClosure :: ProcessId -> Closure (Process ())
reverseClosure pid = closure decoder (encode pid)
  where decoder :: Static (ByteString -> Process ())
        decoder = runQActionStatic `staticCompose` decodeProcessIdStatic

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
    rtable = registerStatic "$runQAction" (toDynamic runQAction)
           . registerStatic "$decodeProcessId" (toDynamic (decode :: ByteString -> ProcessId))
           $ initRemoteTable
