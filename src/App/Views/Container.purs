module App.Views.Container where

import Prelude

import App.Domain.AppState (AppState)
import App.Domain.Route (Route(..))
import App.Views.EditUser (editUser)
import App.Views.Users (users)
import Data.Lens (to)
import React (ReactClass, createClass, createElement, getProps, spec)
import React.DOM as R
import React.DOM.Props as P
import React.Redox (connect)
import Type.Proxy (Proxy(..))

container :: ReactClass Unit
container =
  connect
    (Proxy :: Proxy AppState)
    (to _.route)
    (\_ route _ -> { route })
    (createClass (spec unit renderFn) { displayName = "Container" })
  where
    renderFn this = do
      { route } <- getProps this
      pure $ R.div
        [ P.style
            { display: "flex"
            , flexDirection: "column"
            , padding: "0.4em"
            }
        ]
        [ case route of
            Users -> createElement users unit []
            EditUser userId -> createElement editUser { userId } []
        ]
