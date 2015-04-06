module Handler.Instances (getInstancesR, postInstancesR) where

import Import
import Application.Types
import qualified Data.Map as M
import Control.Concurrent.STM

mkNewId :: App -> STM InstanceId
mkNewId application = do
    ks <- keys application
    return $ InstanceId $ maximum ks + 1

initNewInstance :: App -> InstanceId -> STM ()
initNewInstance application iid = do
  oldState <- readTVar $ socketStates'
  newState   <- newTVar myState
  channel <- newBroadcastTChan
  writeTVar socketStates' (M.insert iid (SocketState newState channel) oldState)
  where socketStates' = socketStates application

unpack :: InstanceId -> Integer
unpack (InstanceId iid) = iid

keys :: App -> STM [Integer]
keys app = do
  readTVar (socketStates app) >>= return . map unpack . M.keys


getInstancesR :: Handler Value
getInstancesR = do
  application <- getYesod
  ks <- liftIO $ atomically $ keys application
  return $ toJSON ks

-- TODO: Think hard about this
postInstancesR :: Handler Value
postInstancesR = do
  application <- getYesod
  liftIO $ atomically $ do
    iid <- mkNewId application
    initNewInstance application iid
    return $ toJSON (unpack iid)
