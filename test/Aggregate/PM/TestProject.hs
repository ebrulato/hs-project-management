{-# LANGUAGE OverloadedStrings #-}

module Aggregate.PM.TestProject (main, lifeOfAProject) where

import Test.HUnit
import Data.Text
import Data.Time
import Data.Time.Calendar
import Data.UUID
import Data.Maybe
import Data.Aeson

import Aggregate.Aggregate
import Aggregate.PM.Project
import DomainEvent.Event


-- createProject :: String -> Text -> Day -> ProjectMode -> IO (Event ProjectId Project, Project)
-- createProject shortName name startDate mode = do

checkCreate :: String -> ProjectId -> String -> Text -> Day -> ProjectMode -> UTCTime -> String -> Test
checkCreate testDesc prjId shortName name startDate mode time val =
    let 
        (event, prj) = createProject prjId shortName name startDate mode time
    in 
    TestCase $ assertEqual testDesc val (show prj)

main :: IO Counts 
main = do
    runTestTT $ TestList [
        serieTest (fromMaybe nil $ fromString "ba7fd9d6-0815-410e-b9f4-91c30244bc7f")
        ]

genUTCTime :: Integer -> Int -> Int -> Integer -> UTCTime
genUTCTime yyyy mm dd ss = UTCTime (fromGregorian yyyy mm dd) (secondsToDiffTime ss)

hour = 60*60 -- s
mn = 60 -- s

serieTest :: ProjectId -> Test
serieTest prjId = TestList [TestLabel "Project Management" $ TestList [ 
    checkCreate "create project" prjId "PMT" "Project Management Test" (fromGregorian 2020 05 20) Kanban (genUTCTime 2020 05 19 (12*hour)) "Project {_projectId = ba7fd9d6-0815-410e-b9f4-91c30244bc7f, _num = 0, _code = \"PMT\", _name = \"Project Management Test\", _description = Nothing, _startDate = 2020-05-20, _endDate = Nothing, _backlog = [], _team = [], _extraDayOff = [], _mode = Kanban, _workload = []}"
    , scenario "create a project and update it" prjId 
    , eventSourcingPrinciple "event sourcing principles" prjId
    ]]

lifeOfAProject :: ProjectId -> ([Event], Project)
lifeOfAProject prjId = 
    let 
        (event0, prj0) = createProject prjId "PMT" "Project Management" (fromGregorian 2020 05 18) Kanban (genUTCTime 2020 05 19 (12*hour))
        (event1, prj1) = updateName prj0 "The Best Project" (genUTCTime 2020 05 19 (12*hour+1*mn))
    in 
    ([event0, event1], prj1)


scenario :: String -> ProjectId -> Test
scenario testDesc prjId = 
    let
        (_, prj) = lifeOfAProject prjId 
    in
    TestCase $ assertEqual 
        testDesc
        "Project {_projectId = ba7fd9d6-0815-410e-b9f4-91c30244bc7f, _num = 1, _code = \"PMT\", _name = \"The Best Project\", _description = Nothing, _startDate = 2020-05-18, _endDate = Nothing, _backlog = [], _team = [], _extraDayOff = [], _mode = Kanban, _workload = []}" 
        (show prj)  

eventSourcingPrinciple :: String -> ProjectId -> Test
eventSourcingPrinciple testDesc prjId = 
    let
        (events, prj) = lifeOfAProject prjId 
        prjSourced = source Nothing events :: Either String Project
    in
    TestCase $ assertEqual 
        testDesc
        (show (Right prj :: Either String Project))  
        (show prjSourced)  
