{-|
Module      : Command.Comand
Description : A definition of a command
Stability   : experimental

A maybe reusable definition of what is a @Command Handler@...

-}
module Command.Command (CommandError, Command(..)) where

import Data.Time (UTCTime)
import Data.UUID (UUID)

import Aggregate.Aggregate (Version)
import DomainEvent.Event (Event)

import Repository.Repository (Repository)

-- | A command error... we have to work the definition of the error code.
data CommandError = CommandError {
    code :: String          -- ^ an error code, should be understandable by a human  
    , msg :: String         -- ^ an error message to help the developpers or the users.
}

-- | A Command is ...
class Command cmd where 
    -- | can be used to perform simple check on the command values (eg. empty string, ...)
    doFilter :: cmd -> Maybe CommandError
    -- | execute the command
    execute :: (Repository repo) => cmd                         -- ^ the command
        -> repo                                                 -- ^ the repository where the aggregate can be found
        -> UTCTime                                              -- ^ the time of the execution of the command
        -> IO (Either CommandError ([Event], Version, UUID))    -- ^ a command error or a t-uplet with a list of the domain event and the new version of the aggregate.
