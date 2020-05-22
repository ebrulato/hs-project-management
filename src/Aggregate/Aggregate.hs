module Aggregate.Aggregate (Aggregate(..), Version) where

import Data.UUID (UUID)

type Version = Int

class Aggregate a where
     aggregateId :: a -> UUID
     version :: a -> Version 
