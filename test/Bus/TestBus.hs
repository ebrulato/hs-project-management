module Bus.TestBus where 

import qualified EventDB.Memory.EventDBMemory as EDB (new)
import qualified Repository.Memory.RepositoryMemory as M
import Repository.Repository

import Check

import Bus.Bus (perform)

main :: IO ()
main = do
    putStrLn "Hello"
    -- let uuid1 = fromMaybe nil $ fromString "ba7fd9d6-0815-410e-b9f4-91c30244bc7f"
    -- eventDB <- EDB.new
    -- repo <- M.new eventDB
    -- result <- fetchProject repo uuid1 10
    -- check (result == Left NO_DATA) "no data"
