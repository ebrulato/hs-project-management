module Command.PM.Create where

import Data.Time (Day)
import Data.Text (Text)

import Aggregate.Aggregate (Aggregate(..))
import Aggregate.PM.Project (createProject, ProjectMode, genProjectId)
import Command.Command (Command(..))

data CommandProjectCreation = CommandProjectCreation {
    code :: String
    , name :: Text
    , start :: Day
    , mode :: ProjectMode
}

instance Command CommandProjectCreation where
    doFilter cmd =
        Nothing
    execute cmd repository time = do 
        projectId <- genProjectId
        let (event, project) = createProject projectId (code cmd) (name cmd) (start cmd) (mode cmd) time
        return $ Right ([event], version project, aggregateId project)