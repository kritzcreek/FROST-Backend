import           Application.Types
import           Handler.Block
import           Handler.Fib
import           Handler.Home
import           Handler.Markdown
import           Handler.Room
import           Handler.Socket
import           Import

import           Control.Concurrent.STM
import           Control.Monad.Logger         (runStderrLoggingT)
import           Control.Monad.Trans.Resource (runResourceT)
import           Database.Persist.Postgresql

mkYesodDispatch "App" resourcesApp

openConnectionCount :: Int
openConnectionCount = 10

connectionString :: ConnectionString
connectionString = "host=localhost port=5432 user=test dbname=test password=test"

main :: IO ()
main = runStderrLoggingT $ withPostgresqlPool connectionString openConnectionCount $ \pool -> liftIO $ do
    runResourceT $ flip runSqlPool pool $ runMigration migrateAll
    state   <- atomically $ newTVar emptyState
    channel <- atomically newBroadcastTChan
    warpEnv $ App pool (SocketState state channel)

{-

Note that warpEnv handles a few important details for us:

* Determines which port to listen on based on environment variables.
* Sets up a number of WAI middlewares, such as request logging.
* Converts our Yesod application into a WAI application.
* Runs the whole thing.

-}
