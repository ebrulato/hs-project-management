module EventDB.EventDB (EventDB(..)) where

import Data.UUID (UUID)

import DomainEvent.Event (Event)
import Aggregate.Aggregate (Version)

class EventDB a where
    add :: a -> [Event] -> UUID -> IO (Either String ())
    events :: a -> UUID -> IO [Event]
    partialEvents :: a -> UUID -> Version -> IO [Event]
