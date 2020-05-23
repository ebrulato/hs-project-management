module Check (check) where

import Control.Exception

check :: Bool -> String -> IO ()
check checked msg = 
    if checked then do 
        putStrLn $ msg ++ " ...ok"
        return ()
    else do 
        putStrLn $ msg ++ " ... KO !!!"
        let res = assert checked () 
        return ()
