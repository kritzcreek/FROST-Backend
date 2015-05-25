module Foundation where

import           Application.Types
import           Control.Concurrent.STM
import           Control.Applicative
import           Data.ByteString.Lazy
import           Data.Map
import           Data.Text (Text ())

import           Database.Persist.Postgresql
import           Yesod
import           Yesod.Static

newtype InstanceId = InstanceId Text deriving(Show, Eq, Ord, Read)
instance PathPiece InstanceId where
  toPathPiece (InstanceId iid) = toPathPiece iid
  fromPathPiece iid = InstanceId <$> fromPathPiece iid


data SocketState = SocketState { appState :: TVar AppState,  broadcastChan :: TChan ByteString }

type SocketStates = Map InstanceId SocketState

data App = App { conpool :: ConnectionPool, socketStates :: TVar SocketStates, getStatic :: Static }

staticFiles "static"

instance Yesod App

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend

    runDB action = do
        App pool _ _ <- getYesod
        runSqlPool action pool

mkYesodData "App" $(parseRoutesFile "config/routes")
