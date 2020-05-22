module Check (check) where

import Control.Exception

check :: Bool -> String -> IO ()
check checked msg = 
    if (assert checked () == ()) then do
        putStrLn $ msg ++ " ...ok"
        return ()
    else 
        return ()