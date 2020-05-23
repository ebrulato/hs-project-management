{-|
Module      : Bus.Bus
Description : The bus :)
Stability   : experimental

A maybe reusable definition of what is a @Bus@...

-}
module Bus.Bus (perform) where

import Data.UUID (UUID)
import Data.Time (UTCTime, getCurrentTime)

import Aggregate.Aggregate (Version)
import EventDB.EventDB (EventDB, add)
import Repository.Repository (Repository)
import DomainEvent.Event (Event)
import Command.Command (Command(..), CommandError)

-- | This function is used to manage a Command in the bus. 
--  TODO 1 => check the behavior if many command are performed at the same time... if we use in memory or if we use
--  a resilient implement of EventDB .
--  * TOOD 2 => maybe we could implement a list of command on the same aggregate in the in the future...  
perform :: (Command cmd, EventDB edb, Repository repo) => cmd   -- ^ the command to execute
    -> edb                                                      -- ^ the Event DB used to store the domain events created by the command
    -> repo                                                     -- ^ a repository where the command can find some aggregate
    -> IO (Either CommandError Version)                         -- ^ an error from the command or the new version of the modified aggregate
perform request eventDB repository = 
    case doFilter request of
        Just error -> return $ Left error 
        Nothing -> do
            time <- getCurrentTime
            resultCmd <- execute request repository time
            case resultCmd of 
                Left error -> return $ Left error
                Right (events, version, key) -> do
                    add eventDB events key
                    return $ Right version  
             