{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}


module Repository.Memory.RepositoryMemory (RepoMemo, Repository.Memory.RepositoryMemory.new) where

import Data.UUID (UUID)
import Data.Aeson (Value)
import Repository.Repository
import Data.HashTable.IO as H

data RepoMemo = RepoMemo {
    hmap :: BasicHashTable UUID Value
    } deriving (Show)

new :: IO RepoMemo
new = do
    hm <- H.new
    return $ RepoMemo hm

instance Repository RepoMemo where
    add repo key val = H.insert (hmap repo) key val
    get repo key = H.lookup (hmap repo) key
    del repo key = H.delete (hmap repo) key    
    put repo key val = do
        old <- get repo key
        case old of
            Nothing -> add repo key val
            Just vold -> do
                del repo key
                add repo key val
