module App.Store where

import Prelude

import App.Action (Action(..), ActionDSL)
import App.Domain.AppState (AppState)
import App.Domain.User (User, UserDetail, UserId(UserId))
import App.Domain.User as User
import App.Domain.Resource (Resource(..))
import App.Domain.Route (Route(..))
import Control.Comonad.Cofree (Cofree, exploreM, head, unfoldCofree)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.History (DocumentTitle(..), URL(..), pushState)
import DOM.HTML.Types (HISTORY)
import DOM.HTML.Window (history)
import Data.Either (Either)
import Data.Foreign (MultipleErrors)
import Data.Foreign.NullOrUndefined (undefined)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Network.HTTP.Affjax (AJAX)
import Redox (Store, hoistCofree', mkIncInterp, mkStoreG, runSubscriptions)
import Unsafe.Coerce (unsafeCoerce)

newtype Run eff a = Run
  { changeRoute :: Route -> Aff eff a
  , setUsersState :: Resource (Array User) -> Aff eff a
  , setUserState :: Resource (Maybe UserDetail) -> Aff eff a
  , fetchUsers :: Aff eff { users :: Either MultipleErrors (Array User), state :: a }
  , fetchUser :: UserId -> Aff eff { user :: Either MultipleErrors (Maybe UserDetail), state :: a }
  }

derive instance functorRun :: Functor (Run eff)

initialState :: AppState
initialState =
  { route: Users
  , users: Empty
  , editingUser: Empty
  }

store :: Store AppState
store = mkStoreG initialState

type Interp eff a = Cofree (Run eff) a

type AppEffects eff = (ajax :: AJAX, console :: CONSOLE, dom :: DOM, history :: HISTORY | eff)

mkInterp :: ∀ e eff. AppState -> Interp (AppEffects eff) AppState
mkInterp state = unfoldCofree id (const next) state
  where
    changeRoute :: Route -> Aff (AppEffects eff) AppState
    changeRoute route = do
      let
        url = case route of
          Users -> ".."
          EditUser Nothing -> "new-user"
          EditUser (Just (UserId name)) -> name
      when (route /= state.route) $ liftEff $
        pushState undefined (DocumentTitle "") (URL url) =<< history =<< window
      pure case route of
        Users -> state { route = route, editingUser = Empty }
        _ -> state { route = route }

    setUsersState :: Resource (Array User) -> Aff (AppEffects eff) AppState
    setUsersState users = pure
      case state.route of
        Users -> state { users = users }
        _ -> state

    setUserState :: Resource (Maybe UserDetail) -> Aff (AppEffects eff) AppState
    setUserState user = pure
      case state.route, user of
        EditUser (Just userId_), Ready editingUser
          | Just userId_ == map (_.name <<< unwrap) editingUser ->
              state { editingUser = user }
        _, _ -> state

    fetchUsers :: Aff (AppEffects eff) { users :: Either MultipleErrors (Array User), state :: AppState }
    fetchUsers = do
      users <- User.fetchUsers
      pure { users, state }

    fetchUser :: UserId -> Aff (AppEffects eff) { user :: Either MultipleErrors (Maybe UserDetail), state :: AppState }
    fetchUser userId = do
      user <- User.fetchUser userId
      pure { user, state }

    next :: Run (AppEffects eff) AppState
    next = Run
      { changeRoute
      , setUsersState
      , setUserState
      , fetchUsers
      , fetchUser
      }

pair :: ∀ x y eff. Action (x -> y) -> Run eff x -> Aff eff y
pair (ChangeRoute route f) (Run interp) = f <$> interp.changeRoute route
pair (SetUsersState users f) (Run interp) = f <$> interp.setUsersState users
pair (SetUserState user f) (Run interp) = f <$> interp.setUserState user
pair (FetchUsers f) (Run interp) = (\{ users, state } -> f users state) <$> interp.fetchUsers
pair (FetchUser userId f) (Run interp) = (\{ user, state } -> f user state) <$> interp.fetchUser userId

runAction :: ∀ e eff
   . ActionDSL (AppState -> AppState)
  -> AppState
  -> Aff (AppEffects eff) AppState
runAction cmds state =
  exploreM pair cmds $
    (logger <<< mkIncInterp store <<< runSubscriptions store <<< mkInterp) state

logger :: ∀ state f
   . Functor f
  => Cofree f state
  -> Cofree f state
logger interp = hoistCofree' nat interp
  where
    nat :: f (Cofree f state) -> f (Cofree f state)
    nat fa = g <$> fa

    g :: Cofree f state -> Cofree f state
    g cof = unsafePerformEff do
      -- Control.Comonad.Cofree.head
      log $ unsafeCoerce (head cof)
      pure cof
