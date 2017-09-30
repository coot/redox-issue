module App.Domain.User where

import Prelude

import Control.Monad.Aff (Aff, delay)
import Data.Either (Either)
import Data.Foreign (MultipleErrors)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, wrap)
import Network.HTTP.Affjax (AJAX)
import Simple.JSON (class ReadForeign, class WriteForeign, readJSON)

fetchUsers :: ∀ fx. Aff (ajax :: AJAX | fx) (Either MultipleErrors (Array User))
fetchUsers = do
  delay (wrap 1000.0)
  pure $ readJSON """[{ "name": "foo" }]"""

fetchUser :: ∀ fx. UserId -> Aff (ajax :: AJAX | fx) (Either MultipleErrors (Maybe UserDetail))
fetchUser userId = do
  delay (wrap 1000.0)
  pure $ readJSON """{ "name": "foo", "detail": "bar" }"""

newtype UserId = UserId String

derive instance ntUserId :: Newtype UserId _
derive newtype instance rfUserId :: ReadForeign UserId
derive newtype instance wfUserId :: WriteForeign UserId
derive newtype instance eqUserId :: Eq UserId
derive newtype instance ordUserId :: Ord UserId

newtype User = User
  { name :: UserId
  }

derive instance ntUser :: Newtype User _
derive instance eqUser :: Eq User
derive newtype instance rfUser :: ReadForeign User

instance ordUser :: Ord User where
  compare a b = name a `compare` name b
    where name (User p) = p.name

newtype UserDetail = UserDetail
  { name :: UserId
  , detail :: String
  }

derive instance eqUserDetail :: Eq UserDetail
derive instance ntUserDetail :: Newtype UserDetail _
derive newtype instance rfUserDetail :: ReadForeign UserDetail
