module Aggregate.User (User, UserId) where

import Data.Text
import Data.Time
import Aggregate.Type.Types (Price, UserId)

data User = User {
    id :: UserId
    , lastName :: Text
    , firstName :: Text
    , email :: Text
    , daysOff :: [Day]
    , cost :: Price
}