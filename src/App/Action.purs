module App.Action where

import Prelude

import App.Domain.AppState (AppState)
import App.Domain.Resource (Resource(..))
import App.Domain.Route (Route)
import App.Domain.User (User, UserDetail, UserId)
import Control.Monad.Free (Free, liftF)
import Data.Array (sort)
import Data.Either (Either, either)
import Data.Foreign (MultipleErrors)
import Data.Maybe (Maybe)
import Debug.Trace (traceAnyA)
import Unsafe.Coerce (unsafeCoerce)

data Action a
  = ChangeRoute Route a
  | SetUsersState (Resource (Array User)) a
  | SetUserState (Resource (Maybe UserDetail)) a
  | FetchUsers (Either MultipleErrors (Array User) -> a)
  | FetchUser UserId (Either MultipleErrors (Maybe UserDetail) -> a)

derive instance functorAction :: Functor Action

type ActionDSL a = Free Action a

changeRoute :: Route -> ActionDSL (AppState -> AppState)
changeRoute route = do
  liftF $ ChangeRoute route unit
  pure id

setUsersState :: Resource (Array User) -> ActionDSL (AppState -> AppState)
setUsersState users = liftF $ SetUsersState users id

setUserState :: Resource (Maybe UserDetail) -> ActionDSL (AppState -> AppState)
setUserState user = do
  _ <- traceAnyA ["setUserState", unsafeCoerce user]
  liftF $ SetUserState user id

fetchUsers' :: ActionDSL (Resource (Array User))
fetchUsers' = do
  usersR <- liftF $ FetchUsers id
  pure $ either Failed (Ready <<< sort) usersR

fetchUsers :: ActionDSL (AppState -> AppState)
fetchUsers = do
  _ <- setUsersState Loading
  users <- fetchUsers'
  setUsersState users

fetchUser' :: UserId -> ActionDSL (Resource (Maybe UserDetail))
fetchUser' userId = do
  userR <- liftF $ FetchUser userId id
  pure $ either Failed Ready userR

fetchUser :: UserId -> ActionDSL (AppState -> AppState)
fetchUser userId = do
  _ <- traceAnyA "fetchUser"
  _ <- setUserState Loading
  user <- fetchUser' userId
  setUserState user
