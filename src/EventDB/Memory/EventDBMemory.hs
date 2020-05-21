module EventDB.Memory.EventDBMemory (EventDBMemo, EventDB.Memory.EventDBMemory.new) where

import Data.UUID (UUID)
import Data.List
import DomainEvent.Event (Event(..))
import EventDB.EventDB (EventDB(..))
import Data.HashTable.IO as H


data EventDBMemo = EventDBMemo {
    hmap :: BasicHashTable UUID [Event]
    } deriving (Show)

new :: IO EventDBMemo
new = do
    hm <- H.new
    return $ EventDBMemo hm

instance EventDB EventDBMemo where
    add db event = do
        let key = (_id event)
        old <- H.lookup (hmap db) key
        case old of
            Nothing -> H.insert (hmap db) key [event]
            Just vold -> do
                H.delete (hmap db) key 
                H.insert (hmap db) key $ vold ++ [event]
    events db key = do
        found <- H.lookup (hmap db) key
        case found of
            Nothing -> return []
            Just evts -> return evts
