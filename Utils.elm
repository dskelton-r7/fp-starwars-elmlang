module Utils where
import Task exposing (Task, ThreadID, fail, succeed, sleep, spawn, sequence, andThen, onError)
import String exposing (toInt)
import Html exposing (..)
import Html.Events exposing (on, targetValue)
import Html.Attributes exposing (..)
import Signal exposing (Address)

linkCss : String -> Html
linkCss path =
  node "link" [ rel "stylesheet", href path ] []


parallel : List (Task error value) -> Task error (List ThreadID)
parallel tasks =
  sequence (List.map spawn tasks)


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
