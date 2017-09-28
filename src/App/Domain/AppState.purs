module App.Domain.AppState where

import App.Domain.User (User, UserDetail)
import App.Domain.Resource (Resource)
import App.Domain.Route (Route)
import Data.Maybe (Maybe)

type AppState =
  { route :: Route
  , users :: Resource (Array User)
  , editingUser :: Resource (Maybe UserDetail)
  }
