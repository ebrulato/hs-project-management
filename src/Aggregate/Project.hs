{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}


module Aggregate.Project (
    Project
    , ProjectId
    , UserStoryId
    , ProjectMode(..)
    , genProjectId
    , createProject
    , updateName
    , sourceProject
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

import qualified Aggregate.Type.Types as T (Price, UserId, AggregateType(..)) 
import DomainEvent.Event (Event(..), genEvent)

type ProjectId = UUID

genProjectId = nextRandom

data Project = Project {
    _projectId :: ProjectId
    , _seqUp :: Int
    , _shortName :: String
    , _name :: Text
    , _description :: Maybe Text
    , _startDate :: Day
    , _endDate :: Maybe Day
    , _backlog :: [UserStory]
    , _team :: [(Role, T.UserId, T.Price)]
    , _extraDayOff :: [Day]
    , _mode :: ProjectMode
    , _workload :: [(Role, T.UserId, Day)] 
} deriving (Show, Generic)
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
} deriving (Show, Generic)
instance ToJSON UserStory
instance FromJSON UserStory

data BusinessValue = Nice | Should | Must deriving (Show, Generic)
instance ToJSON BusinessValue
instance FromJSON BusinessValue

data Complexity = V0_5 | V1 | V2 | V3 | V5 | V8 | V13 | V20 deriving (Show, Generic)
instance ToJSON Complexity
instance FromJSON Complexity

data ProjectMode = Scrum Int | Kanban deriving (Show, Generic)
instance ToJSON ProjectMode
instance FromJSON ProjectMode

data UserStoryState = TODO | RUN | DONE deriving (Show, Generic)
instance ToJSON UserStoryState
instance FromJSON UserStoryState

data ProjectState = PRESALE | SETUP | DEVELOPMENT | HOMOLOGATION | PROD | CLOSED deriving (Show)

data Role = DM | PPO | TL | SM | DEV deriving (Show, Generic)
instance ToJSON Role
instance FromJSON Role

makeLenses ''Project

_updateAndgenEventProject :: Project -> UTCTime -> UpdateProject -> (Event, Project) 
_updateAndgenEventProject prj time payload  =
    let 
        project = applyUpdate prj payload
        event = genEvent (project ^. projectId) T.Project time ((project ^. seqUp) + 1) $ toJSON payload
    in
    (event, over seqUp (+ 1) project)


-- First Operation
-- TODO : Either ? if the prjId is already used ?
createProject :: ProjectId -> String -> Text -> Day -> ProjectMode -> UTCTime -> (Event, Project)
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


-- For All the updates
data UpdateProject = 
    UpdateName Text
    deriving (Show, Generic)
instance ToJSON UpdateProject
instance FromJSON UpdateProject

applyUpdate :: Project -> UpdateProject  -> Project
applyUpdate project update = 
    case update of
        UpdateName newName -> set name newName project


-- Update the Name
updateName :: Project -> Text -> UTCTime -> (Event, Project)
updateName prj newName time =
    _updateAndgenEventProject prj time $ UpdateName newName
    
-- Event sourcing
start = Left "start"

sourceProject :: [Event] -> Either String Project
sourceProject events =
    case checkSequence events of 
        Left pos -> Left $ "Can not reconstruct a Project, the sequence #"++(show pos)++" is missing"
        Right (x:xs) ->
            case fromJSON $ _payload x :: Result Project of
                Error m -> Left $ "Can not recreate the first version of the Project #"++(show $ _id x)++ " ("++m++")"
                Success p -> foldl (\ep e ->
                    case ep of 
                        Right project -> 
                            let 
                                newProject = over seqUp (+ 1) project
                            in 
                            case fromJSON (_payload e) :: Result UpdateProject of
                                Success update -> Right $ applyUpdate newProject update  
                                Error m -> Left $ "Can not decode a Project update at position "++(show $ _seq e)++" ("++m++")" 
                        _ -> ep 
                    ) (Right p) xs
