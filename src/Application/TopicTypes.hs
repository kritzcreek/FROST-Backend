{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Application.TopicTypes where

import Database.Persist.TH

import Data.Aeson
import GHC.Generics

data TopicType = Discusssion | Presentation | Workshop deriving(Show, Read, Eq, Generic)
derivePersistField "TopicType"

instance ToJSON TopicType
instance FromJSON TopicType
