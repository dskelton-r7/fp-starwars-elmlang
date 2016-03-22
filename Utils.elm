module Utils where
import Html exposing (..)
import Html.Attributes exposing (..)

linkCss : String -> Html
linkCss path =
  node "link" [ rel "stylesheet", href path ] []
