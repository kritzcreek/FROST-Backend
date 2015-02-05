{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Socket (openSpaceServer, handleSocketIOR, ServerState (..)) where

import Import

import qualified Data.Text()

import Application.Types
import Application.Engine

import Control.Monad.Trans.Reader
import Control.Applicative
import Control.Monad.Trans.State
import Control.Monad.State.Class
--import Debug.Trace
import qualified Network.SocketIO as SocketIO

import qualified Control.Concurrent.STM as STM

data ServerState = ServerState { appState :: STM.TVar AppState }

readState :: ServerState -> ReaderT SocketIO.Socket IO AppState
readState = liftIO . STM.atomically . STM.readTVar . appState

handleSocketIOR :: Handler ()
handleSocketIOR = do
  app <- getYesod
  socketIoHandler app

openSpaceServer servstate = do
  SocketIO.on "message" $ \ a -> do
    liftIO $ STM.atomically $ do
      newState <- evalAction a <$> STM.readTVar (appState servstate)
      STM.writeTVar (appState servstate) newState
    SocketIO.broadcast "message" a
  SocketIO.on "state" $ do
    mystate <- readState servstate
    SocketIO.emit "state" (generateActions mystate)
  SocketIO.on "commit" $ do
    mystate <- readState servstate
    liftIO $ putStrLn "lol"
    --result <- execute "select * from table" state
    return mystate

