module Handler.Admin where

import           Application.Engine
import           Application.Types
import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Monad          (forever)
import           Data.Aeson
import           Data.ByteString.Lazy
import qualified Data.Text as T
import           Data.Time.Clock
import           Database.Persist.Sql
import           Handler.Socket         (getBroadcastChannel, getServerState)
import           Import
import           Yesod.WebSockets

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

commandResponse ::  Command -> WebSocketsT (HandlerT App IO) ByteString
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


snapshotForm snapshots = renderDivs $ LoadSnapshot
  <$> areq (selectFieldList snapshots) "SnapshotField" Nothing


getAdminR :: Handler Html
getAdminR = do
  time <- liftIO getCurrentTime
  snapshots <- runDB $ selectKeysList [SnapshotTimestamp <. time] []
  ((result, widget), enctype) <- runFormPost $ (snapshotForm $ (\ k -> ("whatever" :: T.Text, k)) <$> snapshots)
  defaultLayout $ do
    setTitle "Admin Console"

    addStylesheetRemote "//cdn.foundation5.zurb.com/foundation.css"

    [whamlet|
      <div .container-fluid>
          <div .row-fluid>
                <h1> Such Admin. Much Console.
                <p> result
                <form method=post action=@{SnapshotR} enctype=#{enctype}>
                  ^{widget}
                  <p>It also doesn't include the submit button.
                  <button>Submit
    |]
