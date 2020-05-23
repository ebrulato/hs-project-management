{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Aggregate.PM.Project
Description : definition of the project aggregate
Stability   : experimental

One of the most important Aggregate of this application.
It manages the main project description, with its backlog, its team and its planned workload.

-}
module Aggregate.PM.Project (
    Project
    , ProjectId
    , ProjectMode(..)
    , genProjectId
    , createProject
    , updateName
    ) where

import Data.Text (Text)
import Data.Time (Day, UTCTime)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Data.Aeson (Value, ToJSON, FromJSON, toJSON, fromJSON, Result(..))
import Control.Lens -- hiding (element)
import Control.Lens.TH
import GHC.Generics (Generic)

import DomainEvent.Event 
import Aggregate.Aggregate (Aggregate(..), Version)
import qualified Aggregate.PM.Types as T (Price, AggregateType(..)) 
import Aggregate.PM.User (UserId)
import DomainEvent.Event (Event(..), genEvent)

type ProjectId = UUID
type DayQuantity = Float

genProjectId = nextRandom

-- | A project is the aggragation of...
data Project = Project {
    _projectId :: ProjectId                         -- ^ the id of the aggregate
    , _num :: Version                               -- ^ the version of the aggregate
    , _code :: String                               -- ^ the project's code name (immutable)
    , _name :: Text                                 -- ^ the complete version of the project name
    , _description :: Maybe Text                    -- ^ a description of the project
    , _startDate :: Day                             -- ^ the first day of the project.
    , _endDate :: Maybe Day                         -- ^ the last day of the project, maybe unknown at the begining of the project.
    , _backlog :: [UserStory]                       -- ^ the backlog of the project, will be used to compute some indicators
    , _team :: [(Role, UserId, T.Price)]          -- ^ the description of the team with the price of each members
    , _extraDayOff :: [Day]                         -- ^ saturday and sunday are dayoff, this list of days will be also ignored.
    , _mode :: ProjectMode                          -- ^ the project mode, kanban or scrum
    , _workload :: [(Role, DayQuantity, T.Price)]   -- ^ the quantity of day for each required role, and the associate sell price.  
} deriving (Show, Generic, Eq)

instance ToJSON Project
instance FromJSON Project

type UserStoryId = Int

data UserStory = UserStory {
    _userId :: UserStoryId
    , _as :: Text
    , _iWant :: Text
    , _inOrderTo :: Text
    , _comment :: Text
    , _businessValue :: BusinessValue
    , _technical :: Complexity
    , _state :: UserStoryState
} deriving (Show, Generic, Eq)
instance ToJSON UserStory
instance FromJSON UserStory

data BusinessValue = Nice | Should | Must deriving (Show, Generic, Eq)
instance ToJSON BusinessValue
instance FromJSON BusinessValue

data Complexity = C0 | C1 | C2 | C3 | C5 | C8 | C13 | C20 deriving (Show, Generic, Eq)
instance ToJSON Complexity
instance FromJSON Complexity

data ProjectMode = Scrum Int | Kanban deriving (Show, Generic, Eq)
instance ToJSON ProjectMode
instance FromJSON ProjectMode

data UserStoryState = TODO | IN_PROGRESS | DONE deriving (Show, Generic, Eq)
instance ToJSON UserStoryState
instance FromJSON UserStoryState

data ProjectState = PRESALE | SETUP | DEVELOPMENT | HOMOLOGATION | PROD | CLOSED deriving (Show)

data Role = DM | PPO | TL | SM | DEV deriving (Show, Generic, Eq)
instance ToJSON Role
instance FromJSON Role

makeLenses ''Project

instance Aggregate Project where
    aggregateId = _projectId
    version = _num
    source = sourceProject


_updateAndgenEventProject :: Project -> UTCTime -> UpdateProject -> (Event, Project) 
_updateAndgenEventProject prj time payload  =
    let 
        project = applyUpdate prj payload
        event = genEvent (project ^. projectId) T.Project time ((project ^. num) + 1) $ toJSON payload
    in
    (event, over num (+ 1) project)


-- | Used to create a Project
createProject :: ProjectId  -- ^ the id of the project, provided by the command handler
    -> String               -- ^ the code name of the project (imutable) 
    -> Text                 -- ^ the fullName of the project
    -> Day                  -- ^ the first day of the project
    -> ProjectMode          -- ^ choose the mode of the project. 
    -> UTCTime              -- ^ the time of the event... Not very interresting
    -> (Event, Project)     -- the Domain Event and the Project created
createProject prjId shortName name startDate mode time =
    let 
        prj = Project prjId 0 shortName name Nothing startDate Nothing [] [] [] mode []
    in
    (genEvent prjId T.Project time 0 (toJSON prj), prj)

resetProject :: Int -> Value -> Either String Project
resetProject pos v = 
    case fromJSON v :: Result Project of
        Success p -> Right p
        Error m -> Left $ "Can not create a project at position "++(show pos)++ " ("++m++")"


-- | Thid solution allows to simplify the implementation of the @sourceProject@
-- But I don't like it.
data UpdateProject = 
    UpdateName Text             -- ^ used to update the name of the project.
    deriving (Show, Generic)
instance ToJSON UpdateProject
instance FromJSON UpdateProject

applyUpdate :: Project -> UpdateProject  -> Project
applyUpdate project update = 
    case update of
        UpdateName newName -> set name newName project

-- | Update the Name
updateName :: Project           -- ^ the project to update 
    -> Text                     -- ^ the new FullName of the project
    -> UTCTime                  -- ^ the time of the update
    -> (Event, Project)         -- ^ the Domain Event and the updated project
updateName prj newName time =
    _updateAndgenEventProject prj time $ UpdateName newName
    
-- A default value for event sourcing case
-- start = Left "start"

-- | Used to recreate a complete project with a list of Domain Event.
-- This function should be call by the Repository module. 
sourceProject :: Maybe Project  -- ^ if @Nothing@ then a projet will be recreate from scratch
    -> [Event]                  -- ^ a list of domain events. 
    -> Either String Project    -- ^ an Error or the Project. 
sourceProject mbProject events =
    case checkSequence events of 
        Left pos -> Left $ "Can not reconstruct a Project, the sequence #"++(show pos)++" is missing"
        Right (x:xs) ->
            case mbProject of 
                Nothing -> 
                    case fromJSON $ _payload x :: Result Project of
                        Error m -> Left $ "Can not recreate the first version of the Project #"++(show $ _id x)++ " ("++m++")"
                        Success p -> foldl sourceEvent (Right p) xs
                Just project -> foldl sourceEvent (Right project) (x:xs)
    where
        sourceEvent ep e = 
            case ep of 
                Right project -> 
                    let 
                        newProject = over num (+ 1) project
                    in 
                    case fromJSON (_payload e) :: Result UpdateProject of
                        Success update -> Right $ applyUpdate newProject update  
                        Error m -> Left $ "Can not decode a Project update at position "++(show $ _sequence e)++" ("++m++")" 
                _ -> ep 
