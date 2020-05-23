
import qualified Aggregate.PM.TestProject
import qualified Repository.TestRepository
import qualified EventDB.TestEventDB
import Test.HUnit

main :: IO Counts
main = do
    Repository.TestRepository.main
    EventDB.TestEventDB.main
    Aggregate.PM.TestProject.main

