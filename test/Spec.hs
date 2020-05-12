
import qualified Aggregate.TestProject
import qualified Repository.TestRepository
import Test.HUnit

main :: IO Counts
main = do
    Repository.TestRepository.main
    Aggregate.TestProject.main

