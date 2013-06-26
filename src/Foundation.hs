module Foundation where

import Yesod
import Yesod.Default.Util
import Data.Default (def)
import Yesod.Form.Jquery

data App = App

instance Yesod App
instance YesodJquery App where
    urlJqueryJs _ = Right "//ajax.googleapis.com/ajax/libs/jquery/1.7/jquery.min.js"
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

mkYesodData "App" $(parseRoutesFile "config/routes")