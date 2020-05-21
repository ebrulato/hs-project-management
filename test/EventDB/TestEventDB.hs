module EventDB.TestEventDB where

import qualified EventDB.Memory.EventDBMemory as E
import qualified EventDB.EventDB as EDB
import Data.Aeson 
import Data.Maybe
import Data.UUID (nil, fromString)
import Aggregate.TestProject (lifeOfAProject)
import Control.Monad
import Control.Exception

check :: Bool -> String -> IO ()
check checked msg = 
    if (assert checked () == ()) then do
        putStrLn $ msg ++ " ...ok"
        return ()
    else 
        return ()

main :: IO ()
main = do
    let uuid1 = fromMaybe nil $ fromString "ba7fd9d6-0815-410e-b9f4-91c30244bc7f"
    db <- E.new
    let (events, project) = lifeOfAProject uuid1
    evts1 <- EDB.events db uuid1
    check (evts1 == []) "no events"
    mapM_ (EDB.add db) events
    evts2 <- EDB.events db uuid1
    check (evts2 == events) "now we have some events"
    