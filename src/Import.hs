module Import
    ( module Import
    , module X
    ) where

import Yesod.Default.Util
import Data.Default
import Language.Haskell.TH
import Yesod as X
import Foundation as X
import Yesod.Form.Jquery as X (urlJqueryJs)

widgetFile :: FilePath -> ExpQ
widgetFile = widgetFileReload def