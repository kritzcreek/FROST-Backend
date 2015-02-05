module Handler.Room where

import           Application.Types
import           Control.Applicative
import           Data.Text           (Text)
import           Import
import           Network.HTTP.Types  (status200)

getRoomsR :: Handler Value
getRoomsR = do
  rs <- runDB $ selectList [RoomCapacity <. Just 200] []
  return $ toJSON $ (\(Entity _ v) -> v) <$> rs

postRoomsR :: Handler Value
postRoomsR = do
  r <- requireJsonBody :: Handler Room
  roomId <- runDB $ insert r
  return $ toJSON roomId

getRoomR :: RoomId -> Handler Value
getRoomR roomId = do
  r <- runDB $ get404 roomId
  return $ toJSON r

postRoomR :: RoomId -> Handler ()
postRoomR roomId = do
  r <- requireJsonBody :: Handler Room
  runDB $ replace roomId r
  sendResponseStatus status200 ("UPDATED" :: Text)
