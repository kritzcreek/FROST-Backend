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
import           Data.Text              (Text (), pack)
import           Data.UUID              (toString)
import           Data.UUID.V4           (nextRandom)
import           Import
import           Network.HTTP.Types
import           System.FilePath.Posix  ((</>))

mkNewId :: Handler InstanceId
mkNewId = do
    newKey <- liftIO nextRandom
    return $ InstanceId (pack (toString newKey))

initNewInstance :: App -> InstanceId -> STM ()
initNewInstance application iid = do
  oldStates <- readTVar socketStates'
  newState <- SocketState <$> newTVar myState <*> newBroadcastTChan
  writeTVar socketStates' (M.insert iid newState oldStates)
  where socketStates' = socketStates application

unwrap :: InstanceId -> Text
unwrap (InstanceId iid) = iid

keys :: App -> STM [Text]
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
  iid <- mkNewId
  newInstanceId <- liftIO $ atomically $ do
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

