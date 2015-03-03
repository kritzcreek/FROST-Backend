module Handler.Admin where

import           Application.Engine
import           Application.Types
import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Monad          (forever)
import           System.Locale
import           Data.Aeson
import           Data.ByteString.Lazy
import           Data.Time.Format
import           Data.Time.Clock
import           Database.Persist.Sql
import           Handler.Socket         (getBroadcastChannel, getServerState)
import           Handler.Snapshot
import           Import
import           Yesod.WebSockets

{-
commandResponse ::  AdminCommand -> WebSocketsT (HandlerT App IO) ByteString
commandResponse PersistSnapshot = do
  serverState <- lift getServerState
  evs <- liftIO . atomically $ generateEvents <$> readTVar serverState
  time <- liftIO getCurrentTime
  key <- lift . runDB . insert $ Snapshot time evs
  return $ encode key
commandResponse (LoadSnapshot key) = do
  (Snapshot _ evs) <- lift . runDB $ get404 key
  serverState <- lift getServerState
  liftIO . atomically $ writeTVar serverState (replayEvents emptyState evs)
  lift getBroadcastChannel >>= liftIO . atomically . flip writeTChan (encode (ReplayEvents evs))
  return ""

adminApp :: WebSocketsT Handler ()
adminApp = forever $ do
    msg <- receiveData
    case decode msg of
      Just cmd -> commandResponse cmd >>= sendTextData
      Nothing -> sendTextData ("Es ist ein Fehler aufgetreten" :: ByteString)
-}

getAdminR :: Handler Html
getAdminR = do
  time <- liftIO getCurrentTime
  snapshots <- runDB $ selectList [SnapshotTimestamp <. time] []
  defaultLayout $ do
    setTitle "Admin Console"

    addStylesheetRemote "//cdn.foundation5.zurb.com/foundation.css"

    [whamlet|
          <div .row>
            <div .large-8 .columns>
              <h1> Such Admin. Much Console.
                <table>
                  <thead>
                    <tr>
                      <th> Key
                      <th> Time
                      <th> Activate
                  <tbody>
                    $forall Entity key ss <- snapshots
                      <tr>
                        <td> #{show (fromSqlKey key)}
                        <td> #{formatTime defaultTimeLocale "%d.%m.%Y %H:%M Uhr" (snapshotTimestamp ss)}
                        <td>
                          <button .button .small> Button
            <div .large-4 .columns>
              <h1> Menu plox
    |]
