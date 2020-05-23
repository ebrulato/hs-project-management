module Repository.Repository where

import Aggregate.Aggregate (Version)
import Aggregate.PM.Project (Project, ProjectId)
import EventDB.EventDB (EventDB)

data RepositoryError = 
    NO_DATA 
    | SEQUENCE_ERROR String
    | VERSION_ERROR
    | DATA_TYPE_ERROR 
    deriving (Eq)

class Repository a where
    fetchProject :: a -> ProjectId -> Version -> IO (Either RepositoryError Project)
