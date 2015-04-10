module Handler.Admin where

import           Application.Types
import           Control.Applicative
import           Control.Concurrent.STM
import           Handler.Instances (keys)
import qualified Data.Text           as T
import           Data.Time.Format    (formatTime)
import           Import
import           System.Locale       (defaultTimeLocale)

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

loadSnapshotFormA :: AForm Handler AdminCommand
loadSnapshotFormA = LoadSnapshot <$> areq (selectField options) "Lade Snapshot" Nothing
  where formatTimestamp = T.pack . formatTime defaultTimeLocale "%d.%m.%Y, %H:%M:%S"
        options = optionsPersistKey [] [Desc SnapshotTimestamp ,LimitTo 5]
          (\k -> formatTimestamp (snapshotTimestamp k))
loadSnapshotForm :: Html -> MForm Handler (FormResult AdminCommand, Widget)
loadSnapshotForm = do
    renderDivs loadSnapshotFormA

getAdminR :: InstanceId -> Handler Html
getAdminR iid = do
  ((_, widget), enctype) <- runFormGet $ loadSnapshotForm
  app <- getYesod
  ks <- liftIO $ atomically $ keys app
  defaultLayout $ do
    setTitle "Admin Console"

    addStylesheetRemote "//cdn.foundation5.zurb.com/foundation.css"

    [whamlet|
      <div .container-fluid>
          <h1> Such Admin. Much Console.
          <div .row-fluid>
             <div .large-6.columns>
              <h2> Snapshots
              <form method=get action=@{SnapshotR iid} enctype=#{enctype}>
                 ^{widget}
                 <button>Snapshot laden
              <form method=post action=@{SnapshotR iid}>
                 <button>Aktuellen Stand speichern
            <div .large-6.columns>
              <h2> Instances
              <ul>
                $forall key <- ks
                  <li>Instance with key #{key}
                    <button> Delete (Later Prolly)
              <form method=post action=@{InstancesR}>
                    <button> Create new Instance
    |]
