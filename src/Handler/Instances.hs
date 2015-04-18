module Handler.Instances
( getInstancesR
, postInstancesR
, getInstanceR
, getInstanceMobileR
, keys
, unwrap) where

import           Application.Types
import           Control.Applicative
import           Control.Concurrent.STM
import qualified Data.Map               as M
import           Data.Text              (Text ())
import           Import
import           Network.HTTP.Types
import           System.FilePath.Posix  ((</>))

mkNewId :: App -> STM InstanceId
mkNewId application = do
    ks <- keys application
    return $ InstanceId (maximum ks + 1)

initNewInstance :: App -> InstanceId -> STM ()
initNewInstance application iid = do
  oldStates <- readTVar socketStates'
  newState <- SocketState <$> newTVar myState <*> newBroadcastTChan
  writeTVar socketStates' (M.insert iid newState oldStates)
  where socketStates' = socketStates application

unwrap :: InstanceId -> Integer
unwrap (InstanceId iid) = iid

keys :: App -> STM [Integer]
keys app = return . map unwrap . M.keys =<< readTVar (socketStates app)

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

getInstance :: FilePath -> InstanceId -> Handler Html
getInstance path instanceId = do
  application <- getYesod
  ks <- liftIO $ atomically $ keys application
  if unwrap instanceId `elem` ks
    then sendFile "text/html" $ "static" </> path </> "index.html"
    else sendResponseStatus status404 ("Instance doesn't exist" :: Text)

getInstanceR :: InstanceId -> Handler Html
getInstanceR = getInstance ""

getInstanceMobileR :: InstanceId -> Handler Html
getInstanceMobileR = getInstance "mobile"

