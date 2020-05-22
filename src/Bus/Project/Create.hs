module Bus.Project.Create where

import Data.Time (Day)
import Data.Text (Text)

import Aggregate.Aggregate (version, aggregateId)
import Aggregate.Project (createProject, ProjectMode, genProjectId)
import Bus.Command 

data CommandCreateProject = CommandCreateProject {
    shortName :: String
    , name :: Text
    , start :: Day
    , mode :: ProjectMode
}

instance Command CommandCreateProject where
    doFilter cmd =
        Nothing
    execute cmd repository time = do 
        projectId <- genProjectId
        let (event, project) = createProject projectId (shortName cmd) (name cmd) (start cmd) (mode cmd) time
        return $ Right ([event], version project, aggregateId project)