module App.Domain.Resource where

import Data.Foreign (MultipleErrors)
import Prelude (class Eq, class Functor)

data Resource a
  = Empty
  | Loading
  | Failed MultipleErrors
  | Ready a

derive instance eqResource :: Eq a => Eq (Resource a)
derive instance fResource :: Functor Resource
