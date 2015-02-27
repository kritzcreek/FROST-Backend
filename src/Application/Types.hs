{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Application.Types where

import           Application.TopicTypes
import qualified Data.Map               as M
import           GHC.Generics
import           Yesod


type Capacity = Int

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Room
    name String
    capacity Capacity Maybe
    deriving Show Eq Ord Generic
Block
    description String
    startHours Int
    startMinutes Int
    endHours Int
    endMinutes Int
    deriving Show Eq Ord Generic
Topic
    description String
    typ TopicType
    deriving Show Eq Generic
Timeslots
    roomId RoomId
    blockId BlockId
    topicId TopicId
    Timeslot roomId blockId
    deriving Show Eq
Snapshot
    events [Event]
|]

instance ToJSON Room
instance FromJSON Room

instance ToJSON Block
instance FromJSON Block

instance ToJSON Topic
instance FromJSON Topic

data Slot = Slot
  { room  :: Room
  , block :: Block
  } deriving (Show, Eq, Ord, Generic)

instance FromJSON Slot
instance ToJSON Slot

data Action = Event | Command

data Command = RequestState | PersistSnapshot | LoadSnapshot SnapshotId | Echo String
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
