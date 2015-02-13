{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
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
    start String
    end String
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
|]

instance ToJSON Room
instance FromJSON Room

instance ToJSON Block
instance FromJSON Block

instance ToJSON Topic
instance FromJSON Topic

---------------
-- | Actions |--
---------------
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
            | ShowError String
            | NOP
            deriving (Show, Eq, Generic)

instance FromJSON Event
instance ToJSON Event

-------------------------
-- | Entire AppState |--
-------------------------

--type Timeslot = (Slot, Topic)

data AppState = AppState { topics    :: [Topic]
                         , rooms     :: [Room]
                         , blocks    :: [Block]
                         , timeslots :: M.Map Slot Topic
                         }
              deriving(Show, Eq, Generic)

--instance ToJSON AppState
--instance FromJSON AppState

 --------------------
 -- | Dummy Values |--
 --------------------
emptyState :: AppState
emptyState = AppState { topics = [], rooms = [], blocks = [], timeslots = M.empty }

myRoom :: Room
myRoom = Room "Frankfurt" (Just 30)
myBlock :: Block
myBlock = Block "Morgens" "9" "12"
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
                      timeslots = (M.insert mySlot myTopic M.empty)}
