module Foundation where

import           Application.Types
import           Control.Concurrent.STM
import           Control.Applicative
import           Data.Map
import           Database.Persist.Postgresql
import           Yesod
import           Yesod.Static


data App = App { conpool :: ConnectionPool, socketStates :: TVar SocketStates, getStatic :: Static }

staticFiles "static"

instance Yesod App

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend

    runDB action = do
        App pool _ _ <- getYesod
        runSqlPool action pool

mkYesodData "App" $(parseRoutesFile "config/routes")
