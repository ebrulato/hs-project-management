module EventDB.TestEventDB where

import Data.Aeson 
import Data.Maybe
import Data.UUID (nil, fromString)
import Control.Monad

import qualified EventDB.Memory.EventDBMemory as E
import qualified EventDB.EventDB as EDB

import Aggregate.PM.TestProject (lifeOfAProject)
import Check



main :: IO ()
main = do
    let uuid1 = fromMaybe nil $ fromString "ba7fd9d6-0815-410e-b9f4-91c30244bc7f"
    db <- E.new
    let (events, project) = lifeOfAProject uuid1
    evts1 <- EDB.events db uuid1
    check (evts1 == []) "no events"
    EDB.add db events uuid1
    evts2 <- EDB.events db uuid1
    check (evts2 == events) "the good events"
    result <- EDB.add db events uuid1
    case result of 
        Left msg -> check True $ "case with duplication of events : " ++ msg
        _ -> check False "case with duplication of events error :("

    