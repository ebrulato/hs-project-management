{-# LANGUAGE DeriveGeneric #-}

module Aggregate.Type.Types (Price, UserId, AggregateType(..), genUserId) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)

type Price = Int

type UserId = UUID

genUserId = nextRandom 

data AggregateType = User | Project deriving (Generic, Eq)

instance ToJSON AggregateType
instance FromJSON AggregateType
