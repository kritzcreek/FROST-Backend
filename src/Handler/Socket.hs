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
import           Data.Text                     (pack)
import           Debug.Trace
import           Import

import           Network.WebSockets.Connection (Connection)
import           Yesod.WebSockets

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Monad                 (forever)
import           Control.Monad.Trans.Reader

getServerState :: Handler (TVar AppState)
getServerState = do
  App _ (SocketState sState _) <- getYesod
  return sState

getBroadcastChannel :: Handler (TChan ByteString)
getBroadcastChannel = do
  App _ (SocketState _ channel) <- getYesod
  return channel

debugger :: Conduit ByteString (ReaderT Connection Handler) ByteString
debugger = CL.mapM $ \ a -> return $ trace (show a) a

parseAction :: Conduit ByteString (ReaderT Connection Handler) Action
parseAction = CL.mapMaybe decode

logAction :: Conduit Action (ReaderT Connection Handler) Action
logAction = CL.mapM $ \ a -> do
    $(logInfo) $ pack (show a)
    return a

applyAction :: Conduit Action (ReaderT Connection (HandlerT App IO)) Action
applyAction = CL.mapM $ \ a -> do
  serverState <- lift getServerState
  liftIO $ atomically $ do
    newState <- evalAction a <$> readTVar serverState
    writeTVar serverState newState
  return a

encodeAction :: Conduit Action (ReaderT Connection Handler) ByteString
encodeAction = CL.mapM $ return . encode

actionConduit :: ConduitM ByteString ByteString (ReaderT Connection Handler) ()
actionConduit = debugger =$= parseAction =$= logAction =$= applyAction =$= encodeAction


openSpaceApp :: WebSocketsT Handler ()
openSpaceApp = do
  writeChan <- lift getBroadcastChannel
  readChan  <- liftIO $ atomically $ dupTChan writeChan
  race_
        (forever $ do
          msg <- liftIO $ atomically (readTChan readChan)
          sendTextData msg)
        (sourceWS $= actionConduit $$ mapM_C (liftIO . atomically . writeTChan writeChan))

handleSocketIOR :: Handler ()
handleSocketIOR = webSockets openSpaceApp
