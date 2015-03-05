{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Socket where

import           Application.Engine
import           Application.Types
import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Monad          (forever)
import           Data.Aeson
import           Data.ByteString.Lazy   (ByteString)
import qualified Data.Text              as T
import           Debug.Trace
import           Import
import           Yesod.WebSockets

getServerState :: Handler (TVar AppState)
getServerState = do
  App _ (SocketState sState _) _ <- getYesod
  return sState

getBroadcastChannel :: Handler (TChan ByteString)
getBroadcastChannel = do
  App _ (SocketState _ channel) _ <- getYesod
  return channel

debugger :: ByteString -> WebSocketsT Handler ()
debugger a = return $ trace (show a) ()

commandResponse :: Command -> WebSocketsT Handler ByteString
commandResponse RequestState = do
  serverState <- lift getServerState
  liftIO $ atomically $ do
    events <- generateEvents <$> readTVar serverState
    return $ encode (ReplayEvents events)


handleCommand :: Command -> WebSocketsT Handler ByteString
handleCommand = commandResponse

logSomething :: Show a => a -> WebSocketsT Handler ()
logSomething a = $(logInfo) $ T.pack (show a)

applyEvent :: Event -> WebSocketsT Handler ()
applyEvent e = do
  serverState <- lift getServerState
  liftIO $ atomically $ do
    newState <- evalEvent e <$> readTVar serverState
    writeTVar serverState newState

handleEvent :: Event -> WebSocketsT Handler ByteString
handleEvent e = do
  logSomething e
  applyEvent e
  return (encode e)

openSpaceApp :: WebSocketsT Handler ()
openSpaceApp = do
  writeChan <- lift getBroadcastChannel
  readChan  <- liftIO $ atomically $ dupTChan writeChan
  race_
        (forever $ do
          msg <- liftIO $ atomically (readTChan readChan)
          sendTextData msg)
        (forever $ do
          action <- receiveData
          logSomething action
          case decode action of
            Just (e :: Event) -> handleEvent e >>= liftIO . atomically . writeTChan writeChan
            Nothing -> case decode action of
              Just (c :: Command) -> handleCommand c >>= sendTextData
              Nothing -> return ())

handleSocketR :: Handler ()
handleSocketR = webSockets openSpaceApp
