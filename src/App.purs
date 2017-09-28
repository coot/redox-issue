module App where

import Prelude

import App.Store (AppEffects, runAction, store)
import App.Views.Container (container)
import Control.Monad.Eff (Eff)
import React (ReactClass)
import React.Redox (withStore)
import Redox as Redox


app :: Eff (AppEffects ()) (ReactClass Unit)
app =
  withStore
    store
    (Redox.dispatch (const $ pure unit) runAction)
    container
