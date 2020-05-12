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

_genEventProject :: Project -> UTCTime -> Int -> Value -> (Event, Project) 
_genEventProject prj time payloadType payload  =
    let 
        event = genEvent (prj ^. projectId) T.Project time ((prj ^. seqUp) + 1) payloadType payload
    in
    (event, over seqUp (+ 1) prj)


-- First Operation
-- TODO : Either ? if the prjId is already used ?
createProject :: ProjectId -> String -> Text -> Day -> ProjectMode -> UTCTime -> (Event, Project)
createProject prjId shortName name startDate mode time =
    let 
        prj = Project prjId 0 shortName name Nothing startDate Nothing [] [] [] mode []
    in
    (genEvent prjId T.Project time 0 0 (toJSON prj), prj)

resetProject :: Int -> Value -> Either String Project
resetProject pos v = 
    case fromJSON v :: Result Project of
        Success p -> Right p
        Error m -> Left $ "Can not create a project at position "++(show pos)++ " ("++m++")"

-- Update the name
data UpdateName = UpdateName {
    oldName :: Text
    , newName :: Text
} deriving (Generic)
instance ToJSON UpdateName
instance FromJSON UpdateName

updateName :: Project -> Text -> UTCTime -> (Event, Project)
updateName prj newName time =
    let 
        (event, newPrj) = _genEventProject prj time 1 $ toJSON $ UpdateName (prj ^. name) newName
    in
    (event, set name newName newPrj)

applyUpdateName :: Either String Project -> Int -> Value -> UTCTime -> Either String Project
applyUpdateName ep pos v time = 
    case ep of 
        Right p -> 
            case fromJSON v :: Result UpdateName of
                Success u -> Right $ snd $ updateName p (newName u) time
                Error m -> Left $ "Can not decode an UpdatName at position "++(show pos)++" ("++m++")" 
        _ -> ep        

-- Event sourcing
start = Left "start"

sourceProject :: [Event] -> Either String Project
sourceProject events =
    case checkSequence events of 
        Left pos -> Left $ "Can not reconstruct a Project, the sequence #"++(show pos)++" is missing"
        Right (x:xs) ->
            case fromJSON $ _payload x :: Result Project of
                Error m -> Left $ "Can no recreate the first version of the Project #"++(show $ _id x)++ " ("++m++")"
                Success p -> foldl (\ep e ->
                    case _payLoadType e of
                        0 -> resetProject (_seq e) $ _payload e 
                        1 -> applyUpdateName ep (_seq e) (_payload e) (_time e)
                        _ -> Left $ "Don't support the payload type at position #"++(show $ _seq e)++" for the project #"++(show $ _id e)   
                    ) (Right p) xs
