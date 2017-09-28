module App.Views.Components.Input where

import React.DOM.Props (style)
import React.Spaces (SpaceM, (!))
import React.Spaces.DOM as R

button :: SpaceM
button = R.input ! style
  { flex: 1
  , height: "100%"
  , fontWeight: 200
  , margin: 0
  , verticalAlign: "middle"

  , border: "none"
  , outline: "none"

  , color: "#666666"
  , backgroundImage: "none"
  , backgroundColor: "transparent"
  , transition: "all 0.15s ease-in-out"
  }
