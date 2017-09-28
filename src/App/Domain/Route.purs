module App.Domain.Route where

import Prelude

import App.Domain.User (UserId)
import Data.Maybe (Maybe)

data Route
  = Users
  | EditUser (Maybe UserId)

derive instance eqRoute :: Eq Route
