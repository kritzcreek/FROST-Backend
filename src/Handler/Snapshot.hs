module Handler.Snapshot where

import           Import
import           Application.Engine
import           Application.Types
import           Control.Concurrent.STM
import           Data.Aeson
import qualified Data.Text as T
import           Data.ByteString.Lazy
import           Data.Time.Clock
import           Control.Applicative
import           Handler.Socket( getServerState, getBroadcastChannel )
import           Handler.Admin

commandResponse :: Command ->  Handler ByteString
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

getSnapshotR :: Handler Html
getSnapshotR = undefined

getSnapshotsR :: Handler Value
getSnapshotsR = undefined

postSnapshotsR :: Handler ()
postSnapshotsR = do
  ((result, _), _) <- runFormPost $ snapshotForm
  case result of
   FormSuccess command -> do
     setMessage "Erfolgreich geladen"
     commandResponse command
     redirect AdminR
   _ -> do
     setMessage "Es ist ein Fehler aufgetreten"
     redirect AdminR
