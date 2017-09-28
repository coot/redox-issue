module App.Views.EditUser where

import Prelude

import App.Action (changeRoute, fetchUser)
import App.Domain.AppState (AppState)
import App.Domain.User (UserDetail(..), UserId)
import App.Domain.Resource (Resource(..))
import App.Domain.Route (Route(..))
import App.Views.Components.Button (buttonStyle)
import Data.Foreign (renderForeignError)
import Data.Lens (to)
import Data.List.NonEmpty (head)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import React (ReactClass, createClass, getProps, spec)
import React.DOM as R
import React.DOM.Props as P
import React.Redox (connect, dispatch)
import Type.Proxy (Proxy(..))


editUser :: ReactClass { userId :: Maybe UserId }
editUser =
  connect
    (Proxy :: Proxy AppState)
    (to \{ editingUser } -> { editingUser })
    (\_ { editingUser } { userId } ->
      { editingUser, userId })
    (createClass $ (spec unit renderFn)
      { displayName = "EditUser"
      , componentDidMount = checkUsers
      })
  where
    checkUsers this = do
      { userId } <- getProps this
      case userId of
        Just userId' -> void $ dispatch this $ fetchUser userId'
        Nothing -> pure unit

    header =
      R.div
        [ P.style
            { display: "flex"
            , flexDirection: "row"
            , justifyContent: "space-between"
            , alignItems: "center"
            }
        ]
        [ R.h1' [ R.text "Edit User" ] ]

    editUserForm this editingUser =
      R.div
        [ P.style
            { display: "flex"
            , flexDirection: "column"
            }
        ]
        [ R.h3' [ R.text $ unwrap editingUser.name ]
        , R.div [ P.style { padding: "0.4em" } ] [ R.text $ "Detail: " <> editingUser.detail ]
        ]

    editUserButtons this =
      R.div
        [ P.style
            { display: "flex"
            , flexDirection: "row"
            , justifyContent: "space-around"
            , margin: "1em"
            }
        ]
        [ R.button
            [ P.style buttonStyle
            -- , P.onClick (\_ -> dispatch this (saveUser editingUserUpdates))
            ]
            [ R.text "Save" ]
        , R.button
            [ P.style buttonStyle
            , P.onClick (\_ -> dispatch this (changeRoute Users))
            ]
            [ R.text "Cancel" ]
        ]

    renderFn this = do
      { editingUser: editingUserM } <- getProps this
      pure $ R.div
        [ P.style
            { display: "flex"
            , flexDirection: "column"
            }
        ]
        $ [ header ]
        <> case editingUserM of
            Ready (Just (UserDetail editingUser)) ->
              [ editUserForm this editingUser
              , editUserButtons this
              ]
            Failed errs -> [ R.text (renderForeignError $ head errs) ]
            _ -> [ R.text "" ]
