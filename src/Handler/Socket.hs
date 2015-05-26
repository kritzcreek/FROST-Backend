{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Socket where

import           Application.Engine
import           Application.Types
import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Monad          (forever)
import           Network.HTTP.Types
import           Data.Aeson
import           Data.ByteString.Lazy   (ByteString)
import qualified Data.Text              as T
import qualified Data.Map               as M
import           Debug.Trace
import           Import
import           Yesod.WebSockets

getServerState :: InstanceId -> Handler (TVar AppState)
getServerState = getFromSocketstate appState

getBroadcastChannel :: InstanceId -> Handler (TChan ByteString)
getBroadcastChannel = getFromSocketstate broadcastChan

getFromSocketstate ::(SocketState -> a) ->  InstanceId -> Handler a
getFromSocketstate accessor instanceId = do
  App _ states _ <- getYesod
  sts <- liftIO $ atomically $ M.lookup instanceId <$> readTVar states
  maybe lookupError return (accessor <$> sts)
  where lookupError = sendResponseStatus status404 ("Instance doesn't exist" :: T.Text)

debugger :: ByteString -> WebSocketsT Handler ()
debugger a = return $ trace (show a) ()

handleCommand :: InstanceId -> Command -> WebSocketsT Handler ByteString
handleCommand instanceId RequestState = do
  serverState <- lift $ getServerState instanceId
  liftIO $ atomically $ do
    events <- generateEvents <$> readTVar serverState
    return $ encode (ReplayEvents events)
handleCommand _ (Echo s) = return (encode s)

logSomething :: Show a => a -> WebSocketsT Handler ()
logSomething a = $(logInfo) $ T.pack (show a)

applyEvent :: InstanceId -> Event -> WebSocketsT Handler ()
applyEvent instanceId e = do
  serverState <- lift $ getServerState instanceId
  liftIO $ atomically $ do
    newState <- evalEvent e <$> readTVar serverState
    writeTVar serverState newState

handleEvent :: InstanceId -> Event -> WebSocketsT Handler ByteString
handleEvent instanceId e = do
  logSomething e
  applyEvent instanceId e
  return (encode e)

openSpaceApp :: InstanceId -> WebSocketsT Handler ()
openSpaceApp instanceId = do
  writeChan <- lift $ getBroadcastChannel instanceId
  readChan  <- liftIO $ atomically $ dupTChan writeChan
  race_
        (forever $ do
          msg <- liftIO $ atomically (readTChan readChan)
          sendTextData msg)
        (forever $ do
          action <- receiveData
          logSomething action
          case decode action of
            Just (e :: Event) -> handleEvent' e >>= liftIO . atomically . writeTChan writeChan
            Nothing -> case decode action of
              Just (c :: Command) -> handleCommand' c >>= sendTextData
              Nothing -> return ())
  where handleCommand' = handleCommand instanceId
        handleEvent' = handleEvent instanceId

handleSocketR :: InstanceId -> Handler ()
handleSocketR instanceId = webSockets $ openSpaceApp instanceId
