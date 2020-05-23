module Repository.TestRepository where

import Data.Aeson 
import Data.Maybe
import Data.UUID (nil, fromString)

import qualified EventDB.Memory.EventDBMemory as EDB (new)
import qualified Repository.Memory.RepositoryMemory as M
import Repository.Repository

import Aggregate.PM.TestProject (lifeOfAProject)
import Check

main :: IO ()
main = do
    let uuid1 = fromMaybe nil $ fromString "ba7fd9d6-0815-410e-b9f4-91c30244bc7f"
    --let uuid2 = fromMaybe nil $ fromString "ba7fd9d6-0815-410e-b9f4-91c30244bc7e"
    eventDB <- EDB.new
    repo <- M.new eventDB
    result <- fetchProject repo uuid1 10
    check (result == Left NO_DATA) "no data"
    
    