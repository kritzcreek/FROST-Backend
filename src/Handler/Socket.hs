{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Handler.Socket where



import           Application.Engine
import           Application.Types
import           Conduit
import           Data.Aeson
import           Data.ByteString.Lazy          (ByteString)
import qualified Data.Conduit.List             as CL
import qualified Data.Text                     as T
import           Debug.Trace
import           Import

import           Yesod.WebSockets

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Monad                 (forever)

getServerState :: Handler (TVar AppState)
getServerState = do
  App _ (SocketState sState _) _ <- getYesod
  return sState

getBroadcastChannel :: Handler (TChan ByteString)
getBroadcastChannel = do
  App _ (SocketState _ channel) _ <- getYesod
  return channel

debugger :: Conduit ByteString (WebSocketsT Handler) ByteString
debugger = CL.mapM $ \ a -> return $ trace (show a) a

parseAction :: Conduit ByteString (WebSocketsT Handler) Action
parseAction = CL.mapMaybe decode

parseCommand :: Conduit ByteString (WebSocketsT Handler) Command
parseCommand = CL.mapMaybe decode

commandResponse :: Conduit Command (WebSocketsT Handler) ByteString
commandResponse = CL.mapM $ buildResponse
  where
    buildResponse RequestState = do
      serverState <- lift getServerState
      liftIO $ atomically $ do
        generateActions <$> readTVar serverState >>= return . encode
    buildResponse (Echo s) = do
      return $ encode s

logAction :: Conduit Action (WebSocketsT Handler) Action
logAction = CL.mapM $ \ a -> do
    $(logInfo) $ T.pack (show a)
    return a

applyAction :: Conduit Action (WebSocketsT Handler) Action
applyAction = CL.mapM $ \ a -> do
  serverState <- lift getServerState
  liftIO $ atomically $ do
    newState <- evalAction a <$> readTVar serverState
    writeTVar serverState newState
  return a

encodeAction :: Conduit Action (WebSocketsT Handler) ByteString
encodeAction = CL.mapM $ return . encode

actionConduit :: ConduitM ByteString ByteString (WebSocketsT Handler) ()
actionConduit = debugger =$= parseAction =$= logAction =$= applyAction =$= encodeAction

--commandConduit :: ConduitM ByteString ByteString (WebSocketsT Handler) ()
--commandConduit = debugger =$= parseE =$= commandResponse

handleAction :: Action -> WebSocketsT Handler ByteString
handleAction a = return (encode a)

handleCommand :: Command -> WebSocketsT Handler ByteString
handleCommand c = return (encode c)

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
          case decode event of
            Just (a :: Action) -> handleAction a >>= liftIO . atomically . writeTChan writeChan
            Nothing -> case decode event of
              Just (c :: Command) -> handleCommand c >>= sendTextData
              Nothing -> return ()
          )
          --sourceWS $= actionConduit $$ mapM_C (liftIO . atomically . writeTChan writeChan))

handleSocketR :: Handler ()
handleSocketR = webSockets openSpaceApp
