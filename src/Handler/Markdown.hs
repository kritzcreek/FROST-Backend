module Handler.Markdown where

import Import
import Text.Markdown
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Data.Text.Lazy (fromStrict)

putMarkdownR :: Handler Value
putMarkdownR = do
    -- Look up the post parameter containing the input Markdown.
    minput <- lookupPostParam "markdown"
    case minput of
        -- If the parameter is not provided, return a 400 error to the user.
        Nothing -> invalidArgs ["No Markdown provided"]
        -- Otherwise, render the Markdown to HTML and return it in a JSON object.
        Just input -> return $ object
            [ "html" .= renderHtml (markdown def $ fromStrict input)
            ]