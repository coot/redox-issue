module App where

import Prelude

import App.Store (AppEffects, initialState, runAction)
import App.Views.Container (container)
import Control.Monad.Eff (Eff)
import React (ReactClass)
import React.Redox (withStore)
import Redox (mkStore)
import Redox as Redox


app :: Eff (AppEffects ()) (ReactClass Unit)
app = do
  store <- mkStore initialState
  withStore
    store
    (Redox.dispatch (const $ pure unit) (runAction store))
    container
