{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Socket where

import           Application.Engine
import           Application.Types
import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Monad          (forever)
import           Control.Monad.Trans.Reader
import           Network.HTTP.Types
import           Data.Aeson
import           Data.ByteString.Lazy   (ByteString)
import qualified Data.Text              as T
import qualified Data.Map               as M
import           Debug.Trace
import           Import
import           Yesod.WebSockets

type ClientHandler = ReaderT InstanceId (WebSocketsT Handler)

getServerState :: InstanceId -> Handler (TVar AppState)
getServerState = getFromSocketstate appState

getBroadcastChannel :: InstanceId -> Handler (TChan ByteString)
getBroadcastChannel = getFromSocketstate broadcastChan

getFromSocketstate ::(SocketState -> a) -> InstanceId -> Handler a
getFromSocketstate accessor instanceId = do
  App _ states _ <- getYesod
  sts <- liftIO $ atomically $ M.lookup instanceId <$> readTVar states
  maybe lookupError return (accessor <$> sts)
  where lookupError = sendResponseStatus status404 ("Instance doesn't exist" :: T.Text)

debugger :: ByteString -> WebSocketsT Handler ()
debugger a = return $ trace (show a) ()

handleCommand :: Command -> ClientHandler ByteString
handleCommand RequestState = do
  serverState <- lift . lift . getServerState =<< ask
  liftIO $ atomically $ do
    events <- generateEvents <$> readTVar serverState
    return $ encode (ReplayEvents events)
handleCommand (Echo s) = return (encode s)

logSomething :: Show a => a -> WebSocketsT Handler ()
logSomething a = $(logInfo) $ T.pack (show a)

applyEvent :: Event -> ClientHandler ()
applyEvent e = do
  serverState <- lift . lift . getServerState =<< ask
  liftIO $ atomically $ do
    newState <- evalEvent e <$> readTVar serverState
    writeTVar serverState newState

handleEvent :: Event -> ClientHandler ByteString
handleEvent e = do
  lift $ logSomething e
  applyEvent e
  return (encode e)

openSpaceApp :: ClientHandler ()
openSpaceApp = do
  writeChan <- lift .lift . getBroadcastChannel =<< ask
  readChan  <- liftIO $ atomically $ dupTChan writeChan
  race_
        (forever $ do
          msg <- liftIO $ atomically (readTChan readChan)
          lift $ sendTextData msg)
        (forever $ do
          action <- lift receiveData
          lift $ logSomething action
          case decode action of
            Just (e :: Event) -> handleEvent e >>= liftIO . atomically . writeTChan writeChan
            Nothing -> case decode action of
              Just (c :: Command) -> handleCommand c >>= lift . sendTextData
              Nothing -> return ())

handleSocketR :: InstanceId -> Handler ()
handleSocketR instanceId = webSockets $ runReaderT openSpaceApp instanceId
