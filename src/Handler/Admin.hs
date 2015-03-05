module Handler.Admin where


import           Application.Types
import           Control.Applicative


import qualified Data.Text              as T

import           Import

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

snapshotFormA :: AForm Handler Command
snapshotFormA = LoadSnapshot <$> areq (selectField options) "Lade Snapshot" Nothing
  where
    options = optionsPersistKey [] [LimitTo 5] (\k -> T.pack $ show (snapshotTimestamp k))

snapshotForm :: Html -> MForm Handler (FormResult Command, Widget)
snapshotForm = do
    renderDivs snapshotFormA

getAdminR :: Handler Html
getAdminR = do
  ((_, widget), enctype) <- runFormPost $ snapshotForm
  defaultLayout $ do
    setTitle "Admin Console"

    addStylesheetRemote "//cdn.foundation5.zurb.com/foundation.css"

    [whamlet|
      <div .container-fluid>
          <div .row-fluid>
                <h1> Such Admin. Much Console.
                <form method=post action=@{SnapshotsR} enctype=#{enctype}>
                  ^{widget}
                  <button> Snapshot laden
    |]
