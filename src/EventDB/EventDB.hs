module EventDB.EventDB (EventDB(..)) where

import DomainEvent.Event (Event)
import Data.UUID (UUID)

class EventDB a where
    add :: a -> Event -> IO ()
    events :: a -> UUID -> IO [Event]
