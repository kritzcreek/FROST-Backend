module Handler.Admin where

import           Application.Types
import           Control.Applicative
import           Control.Concurrent.STM
import           Handler.Instances (getAllKeys)
import qualified Data.Text           as T
import           Data.Time.Format    (formatTime)
import           System.Locale (defaultTimeLocale)
import           Import

renderSemantic :: Monad m => FormRender m a
renderSemantic aform fragment = do
    (res, views') <- aFormToForm aform
    let views = views' []
    let widget = [whamlet|
$newline never
\#{fragment}
$forall view <- views
    <div .ui .field :fvRequired view:.required :not $ fvRequired view:.optional>
        <label for=#{fvId view}>#{fvLabel view}
        $maybe tt <- fvTooltip view
            <div .tooltip>#{tt}
        ^{fvInput view}
        $maybe err <- fvErrors view
            <div .errors>#{err}
|]
    return (res, widget)

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

keySelectField = areq (selectField keyOptions) "Instanz" Nothing
  where keyOptions = mkOptionList <$> (map (\x -> Option x (InstanceId x) x) <$> keys)
        keys = getAllKeys =<< getYesod

loadSnapshotFormA :: AForm Handler AdminCommand
loadSnapshotFormA = LoadSnapshot
                    <$> areq (selectField options) "Lade Snapshot" Nothing
                    <*> keySelectField
  where formatTimestamp = T.pack . formatTime defaultTimeLocale "%d.%m.%Y, %H:%M:%S"
        options = optionsPersistKey [] [Desc SnapshotTimestamp ,LimitTo 5]
          (formatTimestamp . snapshotTimestamp)

loadSnapshotForm :: Html -> MForm Handler (FormResult AdminCommand, Widget)
loadSnapshotForm = renderSemantic loadSnapshotFormA

saveSnapshotFormA :: AForm Handler AdminCommand
saveSnapshotFormA =
  PersistSnapshot <$> keySelectField

saveSnapshotForm :: Html -> MForm Handler (FormResult AdminCommand, Widget)
saveSnapshotForm = renderSemantic saveSnapshotFormA

getAdminR :: Handler Html
getAdminR = do
  ((_, widget), enctype) <- runFormGet loadSnapshotForm
  ((_, newSnapshotWidget), newSnapshotEnctype) <- runFormPost $ saveSnapshotForm
  keys <- getAllKeys =<< getYesod
  defaultLayout $ do
    setTitle "Admin Console"

    addStylesheetRemote "//cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.0.8/semantic.min.css"
    $(widgetFile "admin")

