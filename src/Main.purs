module Main where

import Prelude

import App (app)
import App.Store (AppEffects)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import DOM.Classy.ParentNode (querySelector)
import DOM.HTML (window)
import DOM.HTML.Window (document)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import React (createElement)
import ReactDOM (render)

main :: Eff (AppEffects ()) Unit
main = do
  root <- querySelector (wrap "#app") =<< document =<< window
  app' <- app
  case root of
    Nothing -> log """Failed to mount app.  No "#app" node found."""
    Just root' -> void $ render (createElement app' unit []) root'
