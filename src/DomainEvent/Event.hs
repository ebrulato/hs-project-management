{-# LANGUAGE DeriveGeneric #-}

module DomainEvent.Event (Event(..), genEvent, checkSequence) where

import Data.Time (UTCTime,  getCurrentTime)
import Data.Aeson (Value, ToJSON, FromJSON)
import Data.UUID (UUID)
import Data.List (sort)
import GHC.Generics (Generic)

import Aggregate.Type.Types (AggregateType)

data Event = Event {
    _id :: UUID
    , _aggregate :: AggregateType 
    , _time :: UTCTime
    , _seq :: Int
    , _payload :: Value
} deriving (Generic, Eq)

instance ToJSON Event
instance FromJSON Event

instance Ord Event where
    compare e f = compare (_seq e) (_seq f) 
    (<) e f = (_seq e) < (_seq f)
    (<=) e f = (_seq e) <= (_seq f)
    (>) e f = (_seq e) > (_seq f)
    (>=) e f = (_seq e) >= (_seq f)

genEvent :: UUID -> AggregateType -> UTCTime -> Int -> Value -> Event 
genEvent id aggregate time seq payload =
    Event id aggregate time seq payload 

-- Check that the sequence is complete, and returned it with the correct order for parsing
checkSequence :: [Event] -> Either Int [Event]
checkSequence events =
    let 
        eventsSorted = sort events
        pos = foldl (\pos e -> if (_seq e) == pos then pos + 1 else pos) 0 eventsSorted
    in
    if (pos == length eventsSorted) then
        Right eventsSorted
    else
        Left pos

