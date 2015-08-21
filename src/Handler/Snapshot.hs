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
import Handler.Instances(getAllKeys)
import           Import


commandResponse :: AdminCommand ->  Handler ByteString
commandResponse (PersistSnapshot iid) = do
  serverState <- getServerState iid -- Totally change this!
  evs <- lift $ atomically $ generateEvents <$> readTVar serverState
  time <- liftIO getCurrentTime
  key <- runDB . insert $ Snapshot time evs
  return $ encode key
commandResponse (LoadSnapshot key iid) = do
  (Snapshot _ evs) <- runDB $ get404 key
  getServerState iid >>= lift . atomically . flip writeTVar (replayEvents emptyState evs)
  getBroadcastChannel iid >>= lift . atomically . flip writeTChan (encode (ReplayEvents evs))
  return ""

getSnapshotR :: Handler Value
getSnapshotR = do
  ((result, _), _) <- runFormGet loadSnapshotForm
  case result of
   FormSuccess command -> do
     setMessage "Erfolgreich geladen"
     _ <- commandResponse command
     redirect $ AdminR
   _ -> do
     setMessage "Es ist ein Fehler aufgetreten"
     redirect $ AdminR

postSnapshotR :: Handler ()
postSnapshotR = do
  ((result, _ ), _) <- runFormPost saveSnapshotForm
  case result of
    FormSuccess command -> do
      setMessage "Erfolgreich gespeichert"
      _ <- commandResponse command
      redirect $ AdminR
    _ -> do
      setMessage "Es ist ein Fehler aufgetreten"
      redirect $ AdminR
