module Handler.Snapshot where

import           Application.Engine
import           Application.Types
import           Control.Applicative
import           Control.Concurrent.STM
import           Data.Aeson
import           Data.ByteString.Lazy
import           Data.Time.Clock
import           Handler.Admin
import           Handler.Socket         (getBroadcastChannel, getServerState)
import           Import


commandResponse :: InstanceId -> AdminCommand ->  Handler ByteString
commandResponse iid PersistSnapshot = do
  serverState <- getServerState iid -- Totally change this!
  evs <- lift $ atomically $ generateEvents <$> readTVar serverState
  time <- liftIO getCurrentTime
  key <- runDB . insert $ Snapshot time evs
  return $ encode key
commandResponse iid (LoadSnapshot key) = do
  (Snapshot _ evs) <- runDB $ get404 key
  getServerState iid >>= lift . atomically . flip writeTVar (replayEvents emptyState evs)
  getBroadcastChannel iid >>= lift . atomically . flip writeTChan (encode (ReplayEvents evs))
  return ""

getSnapshotR :: InstanceId -> Handler Value
getSnapshotR iid = do
  ((result, _), _) <- runFormGet loadSnapshotForm
  case result of
   FormSuccess command -> do
     setMessage "Erfolgreich geladen"
     _ <- commandResponse iid command
     redirect $ AdminR iid
   _ -> do
     setMessage "Es ist ein Fehler aufgetreten"
     redirect $ AdminR iid

postSnapshotR :: InstanceId -> Handler ()
postSnapshotR iid = do
  _ <- commandResponse iid PersistSnapshot
  redirect $ AdminR iid
