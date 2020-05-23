
{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : Aggregate.PM.Types
Description : The type of the aggregates supported by this server
Stability   : experimental

Contains the definitions of the common types used in our current project.

-}
module Aggregate.PM.Types (Price, AggregateType(..)) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)

-- | Price allowes to describe the cost of a ressource.
type Price = Int

-- | The types of Aggregates used in our project
data AggregateType = 
    ExternalUser            -- ^ Users are external aggregates provided by an other micro-service. 
    | Project               -- ^ Project are managed by our micro-service
    | Imputation            -- ^ Used to describe the work of an ExternalUser on the Project or other cases... 
    deriving (Generic, Eq)

instance ToJSON AggregateType
instance FromJSON AggregateType
