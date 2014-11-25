module Com (runLocal, thClientProc, rtable, master) where

import Control.Distributed.Process
import Control.Distributed.Process.Debug
import Control.Distributed.Process.Management
import Control.Distributed.Process.Node (initRemoteTable, LocalNode, runProcess)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Static (staticLabel, registerStatic, staticCompose)
import Control.Distributed.Process.Internal.StrictMVar ( newEmptyMVar, putMVar, takeMVar )
import Data.Binary (encode, decode, get)
import           Data.Binary.Get
import           Data.Binary.Put
import Unsafe.Coerce (unsafeCoerce)
import BasicTypes (HValue)
import qualified Language.Haskell.TH.Syntax     as TH

import Data.Rank1Dynamic (toDynamic)
import qualified Data.ByteString as B (ByteString, readFile, writeFile, length)
import Data.ByteString.Lazy as BL (ByteString, fromStrict, toStrict, length)

-- runner (loading, and runQing)
import OOPTH.Data
import OOPTH.Import

import qualified OOPTH.Types as T
import OOPTH.Eval

import           Data.IORef

rtable = registerStatic "$runTHServer" (toDynamic runTHServer)
         . registerStatic "$decodeProcessId" (toDynamic (decode :: ByteString -> ProcessId))
         $ initRemoteTable

master :: Backend -> [NodeId] -> Process ()
master backend slaves = do
  _ <- thClientProc slaves
  log "Stopping Nodes"
  -- Terminate the slaves when the master terminates (this is optional)
  terminateAllSlaves backend
  where
    log = liftIO . putStrLn

runLocalProcess :: LocalNode -> Process a -> IO a
runLocalProcess node proc = do
  result <- newEmptyMVar
  runProcess node $ do
    setTraceFlags $ defaultTraceFlags { traceSend = (Just TraceAll)
                                      , traceRecv = (Just TraceAll)
                                      }
    ret <- proc

    liftIO $ putMVar result ret
  takeMVar result

runLocal :: Backend -> ([NodeId] -> Process a) -> IO a
runLocal backend proc = do
  node <- newLocalNode backend
  -- runLocalProcess :: LocalProcess -> Process a -> IO a
  runLocalProcess node $ do
    slaves <- findSlaves backend
    redirectLogsHere backend slaves
    proc (map processNodeId slaves) -- `finally` shutdownLogger


thClientProc :: [NodeId] -> Process HValue
thClientProc slaves = do
  us <- getSelfPid
  log $ "[Client] Slaves: " ++ show slaves
  let firstSlave = head slaves
  getTraceFlags >>= \flags -> setTraceFlagsRemote flags firstSlave
  _ <- startTraceRelay firstSlave
  pid <- spawn firstSlave (runTHServerClosure us)
  go pid
  where
    log :: String -> Process ()
    log = traceLog
    toHv :: Show a => Get a -> B.ByteString -> Process HValue
    toHv g b = let h = runGet g (BL.fromStrict b)
               in  {- TH.qRunIO (print h) >> -} return (unsafeCoerce h)
    go :: ProcessId -> Process HValue
    go pid = do
      log "[Client] Reading file..."
      payload <- liftIO $ B.readFile "a.out"
      log $ "[Client] Read file: " ++ (show $ B.length payload) ++ " bytes"
      log "[Client] Sending Message..."
      send pid $ (T.SimpleTH T.THExp payload Nothing :: T.Message)
      log "[Client] Awaiting response..."
      msg <- expect :: Process T.Message
      log "[Client] Received a response..."
      case msg of
        T.RunTH' bsr -> do
          log $ "[Client] Received: " ++ (show bsr)
          toHv (get :: Get TH.Exp) bsr
        _ -> do
          log $ "[Client] Received: " ++ (show msg)
          go pid -- recur

-- cloud-haskell-boilerplate
decodeProcessIdStatic :: Static (ByteString -> ProcessId)
decodeProcessIdStatic = staticLabel "$decodeProcessId"

runTHServerStatic :: Static (ProcessId -> Process ())
runTHServerStatic = staticLabel "$runTHServer"

runTHServerClosure :: ProcessId -> Closure (Process ())
runTHServerClosure pid = closure decoder (encode pid)
  where decoder :: Static (ByteString -> Process ())
        decoder = runTHServerStatic `staticCompose` decodeProcessIdStatic
