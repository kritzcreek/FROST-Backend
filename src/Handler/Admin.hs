module Handler.Admin where

import           Application.Engine
import           Application.Types
import           Control.Concurrent.STM
import           Data.Aeson
import qualified Data.Text as T
import           Data.Time.Clock
import           Control.Applicative
import           Import

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

snapshotFormA :: AForm Handler Command
snapshotFormA = LoadSnapshot <$> areq (selectField options) "Lade Snapshot" Nothing
  where
    options = optionsPersistKey [] [LimitTo 5] (\k -> T.pack $ show (snapshotTimestamp k))

snapshotForm = do
    renderDivs snapshotFormA

getAdminR :: Handler Html
getAdminR = do
  ((result, widget), enctype) <- runFormPost $ snapshotForm
  defaultLayout $ do
    setTitle "Admin Console"

    addStylesheetRemote "//cdn.foundation5.zurb.com/foundation.css"

    [whamlet|
      <div .container-fluid>
          <div .row-fluid>
                <h1> Such Admin. Much Console.
                <form method=post action=@{SnapshotsR} enctype=#{enctype}>
                  ^{widget}
                  <p>It also doesn't include the submit button.
                  <button>Submit
    |]
