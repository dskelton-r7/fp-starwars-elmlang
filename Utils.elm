module Utils where
import String exposing (toInt)
import Html exposing (..)
import Html.Events exposing (on, targetValue)
import Html.Attributes exposing (..)
import Signal exposing (Address)

linkCss : String -> Html
linkCss path =
  node "link" [ rel "stylesheet", href path ] []


onInput : Address a -> (String -> a) -> Attribute
onInput address f =
  on "input" targetValue (\v -> Signal.message address (f v))


parseInt : String -> Int
parseInt string =
  case String.toInt string of
    Ok value ->
      value
    Err error ->
      0
