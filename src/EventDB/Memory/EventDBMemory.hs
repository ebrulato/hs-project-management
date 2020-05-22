module EventDB.Memory.EventDBMemory (EventDBMemo, EventDB.Memory.EventDBMemory.new) where

import Data.UUID (UUID)
import Data.List
import DomainEvent.Event (Event(..), checkSequence)
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
    add db events key = do
        old <- H.lookup (hmap db) key
        case old of
            Nothing -> do 
                H.insert (hmap db) key events
                return $ Right ()
            Just vold ->
                case checkSequence (vold ++ events) of 
                    Left _ -> do 
                        return $ Left "At least, two events have the samme sequence value :("
                    Right events -> do
                        H.delete (hmap db) key 
                        H.insert (hmap db) key events
                        return $ Right ()
    events db key = do
        found <- H.lookup (hmap db) key
        case found of
            Nothing -> return []
            Just evts -> return evts
    partialEvents db key ver = do
        found <- H.lookup (hmap db) key
        case found of
            Nothing -> return []
            Just evts -> 
                return $ snd $ break (\e -> _sequence e > ver) $ sort evts
