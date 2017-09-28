module App.Views.Components.Button where

buttonStyle ::
  { display :: String
  , alignItems :: String
  , justifyContent :: String
  , boxSizing :: String
  , fontSize :: String
  , height :: String
  , padding :: String
  , borderWidth :: String
  , borderBottomWidth :: String
  , borderRadius :: String
  , borderBottom :: String
  , color :: String
  , backgroundColor :: String
  , cursor :: String
  , textTransform :: String
  , textdecoration :: String
  }
buttonStyle =
  { display: "flex"
  , alignItems: "center"
  , justifyContent: "center"
  , boxSizing: "border-box"

  , fontSize: "14px"
  , height: "40px"
  , padding: "4px 16px 2px"

  , borderWidth: "0px"
  , borderBottomWidth: "0px"
  , borderRadius: "3px"
  , borderBottom: "3px solid rgba(0, 0, 0, 0.08)"

  , color: "#fff"
  , backgroundColor: "#95d2e9"
  , cursor: "pointer"
  , textTransform: "uppercase"
  , textdecoration: "none"
  }
