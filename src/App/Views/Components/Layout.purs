module App.Views.Components.Layout where

import Prelude hiding (div)

import React.DOM.Props (style)
import React.Spaces (SpaceM, (!))
import React.Spaces.DOM (div)

type Flex style =
  { display :: String
  , flexDirection :: String
  | style
  }

row :: SpaceM -> SpaceM
row =
  div ! style
    { display: "flex"
    , flexDirection: "row"
    }

rowReverse :: SpaceM -> SpaceM
rowReverse =
  div ! style
    { display: "flex"
    , flexDirection: "row-reverse"
    }

row_ :: ∀ style. Flex style -> SpaceM -> SpaceM
row_ s =
  div ! style s
    { display = "flex"
    , flexDirection = "row"
    }

rowReverse_ :: ∀ style. Flex style -> SpaceM -> SpaceM
rowReverse_ s =
  div ! style s
    { display = "flex"
    , flexDirection = "row-reverse"
    }

column :: SpaceM -> SpaceM
column =
  div ! style
    { display: "flex"
    , flexDirection: "column"
    }

columnReverse :: SpaceM -> SpaceM
columnReverse =
  div ! style
    { display: "flex"
    , flexDirection: "column-reverse"
    }

column_ :: ∀ style. Flex style -> SpaceM -> SpaceM
column_ s =
  div ! style s
    { display = "flex"
    , flexDirection = "column"
    }

columnReverse_ :: ∀ style. Flex style -> SpaceM -> SpaceM
columnReverse_ s =
  div ! style s
    { display = "flex"
    , flexDirection = "column-reverse"
    }
