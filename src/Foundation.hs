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

{-

The Yesod typeclass allows us to alter a number of the behaviors of Yesod, such
as the default page layout, error handler functions, and how URLs are rendered.

-}
instance Yesod App where
  cleanPath _ ["socket.io", ""] = Right ["socket.io"]
  cleanPath _ p = Right p

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend

    runDB action = do
        App pool _ _ <- getYesod
        runSqlPool action pool


mkYesodData "App" [parseRoutesNoCheck|
/static          StaticR Static getStatic
/                HomeR     GET
/markdown        MarkdownR PUT
/fib/#Int        FibR      GET
/rooms           RoomsR    GET POST
/blocks          BlocksR   GET POST
/rooms/#RoomId   RoomR     GET POST
/blocks/#BlockId BlockR    GET POST
/socket          SocketR
|]
