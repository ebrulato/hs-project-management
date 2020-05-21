
import qualified Aggregate.TestProject
import qualified Repository.TestRepository
import qualified EventDB.TestEventDB
import Test.HUnit

main :: IO Counts
main = do
    Repository.TestRepository.main
    EventDB.TestEventDB.main
    Aggregate.TestProject.main

