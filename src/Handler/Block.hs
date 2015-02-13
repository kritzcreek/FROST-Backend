module Handler.Block where

import           Application.Types
import           Control.Applicative
import           Data.Text           (Text)
import           Import
import           Network.HTTP.Types

getBlocksR :: Handler Value
getBlocksR = do
  bs <- runDB $ selectList [] [] :: Handler [Entity Block]
  return $ toJSON $ (\ (Entity _ v) -> v) <$> bs

postBlocksR :: Handler Value
postBlocksR = do
  b <- requireJsonBody :: Handler Block
  postId <- runDB $ insert b
  return $ toJSON postId

getBlockR :: BlockId -> Handler Value
getBlockR blockId = do
  b <- runDB $ get404 blockId
  return $ toJSON b

postBlockR :: BlockId -> Handler ()
postBlockR blockId = do
  b <- requireJsonBody :: Handler Block
  runDB $ replace blockId b
  sendResponseStatus status200 ("UPDATED" :: Text)
