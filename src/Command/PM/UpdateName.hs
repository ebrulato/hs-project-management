module Command.PM.UpdateName where

import Data.Text (Text)

import Aggregate.Aggregate (Version, Aggregate(..))
import Aggregate.PM.Project (updateName, ProjectId)
import Repository.Repository (fetchProject)
import Command.Command (Command(..))

data CommandProjectUpdateName = CommandProjectUpdateName {
    projectId :: ProjectId
    , num :: Version 
    , newName :: Text
}

instance Command CommandProjectUpdateName where
    doFilter cmd =
        Nothing
    execute cmd repository time = do 
        project <- fetchProject repository (projectId cmd) (num cmd)
        let (event, project) = updateName project (newName cmd) time
        return $ Right ([event], version project, aggregateId project)