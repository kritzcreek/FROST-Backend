module Foundation where

import           Application.Types
import           Control.Concurrent.STM
import           Data.ByteString.Lazy
import           Database.Persist.Postgresql
import           Yesod
import           Yesod.Static

data SocketState = SocketState { appState :: TVar AppState,  broadcastChan :: TChan ByteString }
data App = App { conpool :: ConnectionPool, socketState :: SocketState, getStatic :: Static }

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
/socket                     SocketR
/admin                      AdminR     GET
/admin/snapshot            SnapshotR GET POST
/                           StaticR Static getStatic
|]
