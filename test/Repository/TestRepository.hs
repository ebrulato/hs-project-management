module Repository.TestRepository where

import qualified Repository.Memory.RepositoryMemory as M
import Repository.Repository
import Data.Aeson 
import Data.Maybe
import Data.UUID (nil, fromString)
import Aggregate.TestProject (lifeOfAProject)

main :: IO ()
main = do
    let uuid1 = fromMaybe nil $ fromString "ba7fd9d6-0815-410e-b9f4-91c30244bc7f"
    --let uuid2 = fromMaybe nil $ fromString "ba7fd9d6-0815-410e-b9f4-91c30244bc7e"
    repo <- M.new
    putStrLn $ "REPO1 =" ++ (show repo)
    let (events, project) = lifeOfAProject uuid1
    add repo uuid1 (toJSON project)
    putStrLn $ "REPO2 =" ++ (show repo)
    