module Foundation where

import           Application.Types
import           Control.Concurrent.STM
import           Control.Applicative
import           Data.ByteString.Lazy
import           Data.Map

import           Database.Persist.Postgresql
import           Yesod
import           Yesod.Static

newtype InstanceId = InstanceId Integer deriving(Show, Eq, Ord, Read)
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

mkYesodData "App" [parseRoutesNoCheck|
/rooms                      RoomsR     GET POST
/blocks                     BlocksR    GET POST
/rooms/#RoomId              RoomR      GET POST
/blocks/#BlockId            BlockR     GET POST
/#InstanceId/socket         SocketR
/admin                      AdminR     GET
/instances                  InstancesR GET POST
/admin/snapshot             SnapshotR GET POST
/                           StaticR Static getStatic
|]
