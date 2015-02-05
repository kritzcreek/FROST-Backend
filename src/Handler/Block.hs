module Handler.Block where

import Import
import Application.Types
import Control.Applicative
import Network.HTTP.Types
import Data.Text(Text)

getBlocksR :: Handler Value
getBlocksR = do
  blocks <- runDB $ selectList [] [] :: Handler [Entity Block]
  return $ toJSON $ (\ (Entity _ v) -> v) <$> blocks

postBlocksR :: Handler Value
postBlocksR = do
  block <- requireJsonBody :: Handler Block
  postId <- runDB $ insert block
  return $ toJSON $ postId

getBlockR :: BlockId -> Handler Value
getBlockR blockId = do
  block <- runDB $ get404 blockId
  return $ toJSON block

postBlockR :: BlockId -> Handler ()
postBlockR blockId = do
  block <- requireJsonBody :: Handler Block
  runDB $ replace blockId block
  sendResponseStatus status200 ("UPDATED" :: Text)
