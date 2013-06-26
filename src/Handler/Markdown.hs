module Handler.Markdown where

import Import
import Text.Markdown
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Data.Text.Lazy (fromStrict)

putMarkdownR :: Handler Value
putMarkdownR = do
    input <- runInputPost $ ireq textField "markdown"
    return $ object ["html" .= renderHtml (markdown def $ fromStrict input)]