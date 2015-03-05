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

commandResponse :: AdminCommand ->  Handler ByteString
commandResponse PersistSnapshot = do
  serverState <- getServerState
  evs <- lift $ atomically $ generateEvents <$> readTVar serverState
  time <- liftIO getCurrentTime
  key <- runDB . insert $ Snapshot time evs
  return $ encode key
commandResponse (LoadSnapshot key) = do
  (Snapshot _ evs) <- runDB $ get404 key
  serverState <- getServerState
  liftIO . atomically $ writeTVar serverState (replayEvents emptyState evs)
  getBroadcastChannel >>= lift . atomically . flip writeTChan (encode (ReplayEvents evs))
  return ""

getSnapshotR :: Handler Value
getSnapshotR = do
  ((result, _), _) <- runFormGet loadSnapshotForm
  case result of
   FormSuccess command -> do
     setMessage "Erfolgreich geladen"
     _ <- commandResponse command
     redirect AdminR
   _ -> do
     setMessage "Es ist ein Fehler aufgetreten"
     redirect AdminR

postSnapshotR :: Handler ()
postSnapshotR = do
  _ <- commandResponse PersistSnapshot
  redirect AdminR
