{-|
Module      : Aggregate.PM.User
Description : External User supported in this micro-service
Stability   : experimental

Extenal Users are defined in an other micro-service.
These information came from an other system

-}
module Aggregate.PM.User (User, UserId) where

import Data.Text
import Data.Time
import Aggregate.PM.Types (Price)

-- | A User Aggregate is very simple
data User = User {
    id :: UserId            
    , lastName :: Text 
    , firstName :: Text
    , email :: Text
    , cost :: Price         -- ^ the cost for a full day of work
}

-- | As User are external component, we use only a String version of its original key
type UserId = String
