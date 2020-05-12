module Repository.Repository where

import Data.UUID (UUID)
import Data.Aeson (Value)

class Repository a where
    add :: a -> UUID -> Value -> IO ()
    get :: a -> UUID -> IO (Maybe Value)
    del :: a -> UUID -> IO ()
    put :: a -> UUID -> Value -> IO ()

