{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}

module Application.Types where

import           Application.TopicTypes
import qualified Data.Map               as M
import           Data.Time.Clock        (UTCTime)
import           GHC.Generics
import           Yesod
import           Data.Text (Text ())
import           Data.ByteString.Lazy
import           Control.Concurrent.STM
import Control.Applicative
import           Database.Persist.Quasi

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
  $(persistFileWith lowerCaseSettings "config/model")

data Slot = Slot
  { room  :: Room
  , block :: Block
  } deriving (Show, Eq, Ord, Generic)

instance FromJSON Slot
instance ToJSON Slot

data Action = Event | Command

data Command = RequestState | Echo String
  deriving (Show, Eq, Generic)

instance FromJSON Command
instance ToJSON Command

data Event = AddTopic Topic
           | DeleteTopic Topic
           | AddRoom Room
           | DeleteRoom Room
           | AddBlock Block
           | DeleteBlock Block
           | AssignTopic Slot Topic
           | UnassignTopic Topic
           | ReplayEvents [Event]
           | NOP
           deriving (Show, Eq, Generic)

instance FromJSON Event
instance ToJSON Event

derivePersistFieldJSON "Event"

data AppState = AppState { topics    :: [Topic]
                         , rooms     :: [Room]
                         , blocks    :: [Block]
                         , timeslots :: M.Map Slot Topic
                         }
              deriving(Show, Eq, Generic)

newtype InstanceId = InstanceId Text deriving(Show, Eq, Ord, Read)
instance PathPiece InstanceId where
  toPathPiece (InstanceId iid) = toPathPiece iid
  fromPathPiece iid = InstanceId <$> fromPathPiece iid


data SocketState = SocketState { appState :: TVar AppState,  broadcastChan :: TChan ByteString }

type SocketStates = M.Map InstanceId SocketState

data AdminCommand = PersistSnapshot InstanceId | LoadSnapshot SnapshotId InstanceId
  deriving (Show, Eq, Generic)

emptyState :: AppState
emptyState = AppState [] [] [] M.empty

myRoom :: Room
myRoom = Room "Frankfurt" (Just 30)
myBlock :: Block
myBlock = Block "Morgens" 9 0 12 0
mySlot :: Slot
mySlot = Slot myRoom myBlock
myTopic :: Topic
myTopic = Topic "Ein Thema" Presentation
myEvent :: Event
myEvent = AssignTopic mySlot myTopic
myState :: AppState
myState = AppState { topics = [myTopic],
                      rooms = [myRoom],
                      blocks = [myBlock],
                      timeslots = M.insert mySlot myTopic M.empty}
