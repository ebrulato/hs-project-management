module Bus.Command where

import Data.UUID (UUID)
import Data.Time (UTCTime, getCurrentTime)

import Aggregate.Aggregate (Version)
import EventDB.EventDB (EventDB, add)
import Repository.Repository (Repository)
import DomainEvent.Event (Event)

data CommandError = CommandError {
    code :: Int -- TODO 
    , msg :: String
}

class Command cmd where 
    doFilter :: cmd -> Maybe CommandError
    execute :: (Repository repo) => cmd -> repo -> UTCTime -> IO (Either CommandError ([Event], Version, UUID))

perform :: (Command cmd, EventDB edb, Repository repo) => cmd -> edb -> repo -> IO (Either CommandError Version)
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
             