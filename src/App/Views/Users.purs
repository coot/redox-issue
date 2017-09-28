module App.Views.Users where

import Prelude

import App.Action (changeRoute, fetchUsers)
import App.Domain.AppState (AppState)
import App.Domain.User (User(..))
import App.Domain.Resource (Resource(..))
import App.Domain.Route (Route(..))
import App.Views.Components.Button (buttonStyle)
import Data.Foreign (renderForeignError)
import Data.Lens (to)
import Data.List.NonEmpty (head)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import React (ReactClass, createClass, createElement, getProps, readState, spec, writeState)
import React.DOM as R
import React.DOM as RD
import React.DOM.Props as P
import React.Redox (connect, dispatch)
import Type.Proxy (Proxy(..))

users :: ReactClass Unit
users =
  connect
    (Proxy :: Proxy AppState)
    (to _.users)
    (\_ users _ -> { usersR: users })
    (createClass (spec unit renderFn)
      { displayName = "Users"
      , componentDidMount = checkUsers
      })
  where
    checkUsers this = do
      { usersR } <- getProps this
      case usersR of
        Empty -> void $ dispatch this fetchUsers
        _ -> pure unit

    header this =
      R.div
        [ P.style
            { display: "flex"
            , flexDirection: "row"
            , justifyContent: "space-between"
            , alignItems: "center"
            }
        ]
        [ newUserTitle
        , newUserButton this
        ]
      where
        newUserTitle = R.h1' [ R.text "Users" ]

        newUserButton this =
          R.button
            [ P.style buttonStyle { height = "30px", padding = "2px 8px 1px" }
            , P.onClick (\_ -> dispatch this (changeRoute $ EditUser Nothing))
            ]
            [ R.text "New User"
            ]

    buildUserList this = do
      { usersR } <- getProps this
      pure case usersR of
        Empty -> R.div' [ R.text "" ]
        Loading -> R.div' [ R.text "Loading..." ]
        Failed errs -> R.div' [ R.text (renderForeignError $ head errs) ]
        Ready users ->
          RD.div
            [ P.style
                { display: "flex"
                , flexDirection: "column"
                , cursor: "pointer"
                , borderTop: "solid 0.01em lightgrey"
                }
            ]
            $ users <#> \user@(User { name }) ->
                createElement userRow { key: name, user } []

    renderFn this = do
      userList <- buildUserList this
      pure $
        R.div'
          [ header this
          , userList
          ]

userRow :: ∀ props. ReactClass { user :: User | props }
userRow =
  createClass (spec init renderFn) { displayName = "UserRow" }
  where
    init = { isHovered: false }
    onHover this _ = writeState this { isHovered: true }
    onUnhover this _ = writeState this { isHovered: false }

    renderFn this = do
      { user: User user } <- getProps this
      { isHovered } <- readState this
      pure $
        R.div
          [ P.style
              { display: "flex"
              , flexDirection: "row"
              , cursor: "pointer"
              , padding: "0.4em"
              , borderBottom: "solid 0.01em lightgrey"
              , backgroundColor:
                  if isHovered
                  then "#eeeeee"
                  else "inherit"
              }
          , P.onMouseEnter (onHover this)
          , P.onMouseLeave (onUnhover this)
          , P.onClick (\_ -> dispatch this (changeRoute $ EditUser (Just user.name)))
          ]
          [ R.text $ unwrap user.name ]
