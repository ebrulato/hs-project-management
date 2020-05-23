{-# LANGUAGE DeriveGeneric #-}

module DomainEvent.Event (Event(..), genEvent, checkSequence) where

import Data.Time (UTCTime,  getCurrentTime)
import Data.Aeson (Value, ToJSON, FromJSON)
import Data.UUID (UUID)
import Data.List (sort)
import GHC.Generics (Generic)

import Aggregate.PM.Types (AggregateType) -- TODO remove this reference to keep the Event definition pure.

data Event = Event {
    _id :: UUID
    , _aggregate :: AggregateType 
    , _time :: UTCTime
    , _sequence :: Int
    , _payload :: Value
} deriving (Generic, Eq)

instance ToJSON Event
instance FromJSON Event

instance Ord Event where
    compare e f = compare (_sequence e) (_sequence f) 
    (<) e f = (_sequence e) < (_sequence f)
    (<=) e f = (_sequence e) <= (_sequence f)
    (>) e f = (_sequence e) > (_sequence f)
    (>=) e f = (_sequence e) >= (_sequence f)

genEvent :: UUID -> AggregateType -> UTCTime -> Int -> Value -> Event 
genEvent id aggregate time seq payload =
    Event id aggregate time seq payload 

-- Check that the sequence is complete, and returned it with the correct order for parsing
-- the sequence could be partial (fot partial update case)
-- the sequence could be empty
-- TODO : check the aggregate type
checkSequence :: [Event] -> Either () [Event]
checkSequence events =
    let 
        eventsSorted = sort events
        firstVersion = if length eventsSorted == 0 then 0 else _sequence $ head events  
        lastVersion = if length eventsSorted == 0 then 0 else _sequence $ last events
        (foundedVersion, pos) = (foldl (\(ver, pos) e -> if (_sequence e) == ver then (ver + 1, pos +1) else (ver, pos)) (firstVersion, 0) eventsSorted)
    in
    if pos == length events then
        Right eventsSorted
    else
        Left ()

