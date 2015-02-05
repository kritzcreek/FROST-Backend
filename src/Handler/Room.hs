module Handler.Room where

import Import
import Application.Types
import Control.Applicative
import Data.Text(Text)
import Network.HTTP.Types (status200)

getRoomsR :: Handler Value
getRoomsR = do
  rooms <- runDB $ selectList [RoomCapacity <. Just 200] []
  return $ toJSON $ (\(Entity _ v) -> v) <$> rooms

postRoomsR :: Handler Value
postRoomsR = do
  room <- requireJsonBody :: Handler Room
  roomId <- runDB $ insert room
  return $ toJSON roomId

getRoomR :: RoomId -> Handler Value
getRoomR roomId = do
  room <- runDB $ get404 roomId
  return $ toJSON room

postRoomR :: RoomId -> Handler ()
postRoomR roomId = do
  room <- requireJsonBody :: Handler Room
  runDB $ replace roomId room
  sendResponseStatus status200 ("UPDATED" :: Text)
