module Handler.Instances (getInstancesR, postInstancesR, getInstanceR, keys, unpack) where

import Import
import Application.Types
import qualified Data.Map as M
import Data.Text (Text())
import Control.Concurrent.STM
import Network.HTTP.Types
import System.FilePath.Posix((</>))

mkNewId :: App -> STM InstanceId
mkNewId application = do
    ks <- keys application
    return $ InstanceId (maximum ks + 1)

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
  newInstanceId <- liftIO $ atomically $ do
    iid <- mkNewId application
    initNewInstance application iid
    return iid
  redirect $ InstanceR newInstanceId

getInstanceR :: InstanceId -> Handler Html
getInstanceR instanceId = do
  application <- getYesod
  ks <- liftIO $ atomically $ keys application
  if unpack instanceId `elem` ks
    then sendFile "text/html" $ "static" </> "index.html"
    else sendResponseStatus status404 ("Instance doesn't exist" :: Text)
