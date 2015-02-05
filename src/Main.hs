import           Handler.Fib
import           Handler.Home
import           Handler.Markdown
import           Handler.Socket(openSpaceServer, handleSocketIOR, ServerState (..))
import           Handler.Room
import           Handler.Block
import           Import
import           Application.Types

import Database.Persist.Postgresql
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runStderrLoggingT)
import Control.Applicative
import qualified Control.Concurrent.STM as STM

import qualified Network.SocketIO as SocketIO
import qualified Network.EngineIO.Yesod as EIO

{-

We've now defined all of our handler functions. The last step is
create a dispatch function which will reference all of them. The
mkYesodDispatch function does this, following the standard naming
scheme we've used in our handler modules to get the appropriate
function names.

This function creates an instance of YesodDispatch. That instance is
used by warpEnv to create an application that the Warp webserver is
able to execute.

-}

mkYesodDispatch "App" resourcesApp

openConnectionCount :: Int
openConnectionCount = 10

connectionString :: ConnectionString
connectionString = "host=localhost port=5432 user=test dbname=test password=test"

main :: IO ()
main = runStderrLoggingT $ withPostgresqlPool connectionString openConnectionCount $ \pool -> liftIO $ do
    runResourceT $ flip runSqlPool pool $ do
        runMigration migrateAll
    state <- ServerState <$> STM.newTVarIO emptyState
    app <- App pool <$> SocketIO.initialize EIO.yesodAPI (openSpaceServer state)
    warpEnv app

{-

Note that warpEnv handles a few important details for us:

* Determines which port to listen on based on environment variables.
* Sets up a number of WAI middlewares, such as request logging.
* Converts our Yesod application into a WAI application.
* Runs the whole thing.

-}