{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}


module Repository.Memory.RepositoryMemory (RepoMemo, Repository.Memory.RepositoryMemory.new) where

import Data.UUID (UUID)
import Data.HashTable.IO as H

import Repository.Repository
import EventDB.EventDB  (EventDB(..))
import Aggregate.Aggregate (Version, version, Aggregate(..))

import Aggregate.PM.Project (Project, ProjectId)

data ValueRepo = Project Project

data (EventDB a) => RepoMemo a = RepoMemo {
    eventDB :: a
    , hmap :: BasicHashTable UUID ValueRepo
    } deriving (Show)

new :: (EventDB a) => a -> IO (RepoMemo a)
new edb = do
    hm <- H.new
    return $ RepoMemo edb hm

instance (EventDB a) => Repository (RepoMemo a) where
    --fetchProject :: (RepoMemo a) -> ProjectId -> Version -> IO (Either RepositoryError Project)
    fetchProject repo projectId ver = do
        current <- H.lookup (hmap repo) projectId
        case current of
            Nothing -> do 
                events <- events (eventDB repo) projectId
                if length events == 0 then return $ Left NO_DATA 
                else case source Nothing events :: Either String Project of
                    Left msg -> return $ Left $ SEQUENCE_ERROR msg 
                    Right project -> do 
                        H.insert (hmap repo) projectId $ Project project 
                        if version project == ver then 
                            return $ Right project
                        else 
                            return $ Left VERSION_ERROR
            Just (Project curProject) -> do
                events <- partialEvents (eventDB repo) projectId (version curProject) 
                if (length events > 0) then do
                    H.delete (hmap repo) projectId 
                    case source (Just curProject) events :: Either String Project of
                        Left msg -> return $ Left $ SEQUENCE_ERROR msg 
                        Right project -> do 
                            H.insert (hmap repo) projectId $ Project project 
                            if version project == ver then 
                                return $ Right project
                            else 
                                return $ Left VERSION_ERROR
                else 
                    if version curProject == ver then 
                        return $ Right curProject
                    else 
                        return $ Left VERSION_ERROR
            --Just _ -> return $ Left DATA_TYPE_ERROR



    -- add repo key val = 
    -- get repo key = 
    -- del repo key = H.delete (hmap repo) key    
    -- put repo key val = do
    --     old <- get repo key
    --     case old of
    --         Nothing -> add repo key val
    --         Just vold -> do
    --             del repo key
    --             add repo key val
