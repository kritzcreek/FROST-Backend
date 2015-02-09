{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
module Handler.Socket where



import           Application.Engine
import           Application.Types
import           Data.Aeson
import           Data.ByteString.Lazy   (ByteString)
import qualified Data.Text              as T
import           Debug.Trace
import           Import

import           Yesod.WebSockets

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Monad          (forever)

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

commandResponse :: Command -> (WebSocketsT Handler) ByteString
commandResponse RequestState = do
      serverState <- lift getServerState
      liftIO $ atomically $ do
        generateActions <$> readTVar serverState >>= return . encode
commandResponse (Echo s) = do
      return $ encode s

handleCommand :: Command -> WebSocketsT Handler ByteString
handleCommand = commandResponse

logAction :: Action -> WebSocketsT Handler ()
logAction a = $(logInfo) $ T.pack (show a)

applyAction :: Action -> WebSocketsT Handler ()
applyAction a = do
  serverState <- lift getServerState
  liftIO $ atomically $ do
    newState <- evalAction a <$> readTVar serverState
    writeTVar serverState newState

handleAction :: Action -> WebSocketsT Handler ByteString
handleAction a = do
  logAction a
  applyAction a
  return (encode a)

openSpaceApp :: WebSocketsT Handler ()
openSpaceApp = do
  writeChan <- lift getBroadcastChannel
  readChan  <- liftIO $ atomically $ dupTChan writeChan
  race_
        (forever $ do
          msg <- liftIO $ atomically (readTChan readChan)
          sendTextData msg)
        (forever $ do
          event <- receiveData
          debugger event
          case decode event of
            Just (a :: Action) -> handleAction a >>= liftIO . atomically . writeTChan writeChan
            Nothing -> case decode event of
              Just (c :: Command) -> handleCommand c >>= sendTextData
              Nothing -> return ()
          )

handleSocketR :: Handler ()
handleSocketR = webSockets openSpaceApp
