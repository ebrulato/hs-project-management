{-|
Module      : Aggregate.Aggregate
Description : definition of an Aggregate
Stability   : experimental

A maybe reusable definition of what is an @Aggregate@...

-}
module Aggregate.Aggregate (Aggregate(..), Version) where

import Data.UUID (UUID)

import DomainEvent.Event (Event)

-- | A Version as a simple integer
type Version = Int

-- | Definition of the Aggregate behaviors
class Aggregate a where
     -- | the id of and Aggregate
     aggregateId :: a -> UUID      
     -- | an aggregate has a version based on its last update.
     version :: a -> Version
     -- | a way to source the aggregate from a list of events
     source :: Maybe a             -- ^ if you provide an Aggregate, this funtion will try to apply the event on it 
          -> [Event]               -- ^ a list of Event
          -> Either String a       -- ^ an error message of the Aggregate updated.

          
