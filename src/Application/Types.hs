{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveGeneric              #-}
module Application.Types where

import           GHC.Generics
import           Application.TopicTypes
import           Yesod
import qualified Data.Map as M

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
  { room :: Room
  , block :: Block
  } deriving (Show, Eq, Ord, Generic)

instance FromJSON Slot
instance ToJSON Slot

data Action = AddTopic Topic
            | DeleteTopic Topic
            | AddRoom Room
            | DeleteRoom Room
            | AddBlock Block
            | DeleteBlock Block
            | AssignTopic Slot Topic
            | UnassignTopic Topic
            | ShowError String
            | NOP
            deriving (Show, Eq, Generic)

instance FromJSON Action
instance ToJSON Action

-------------------------
-- | Entire AppState |--
-------------------------

--type Timeslot = (Slot, Topic)

data AppState = AppState { topics :: [Topic]
                         , rooms :: [Room]
                         , blocks :: [Block]
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

