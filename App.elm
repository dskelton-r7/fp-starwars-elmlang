module App where

import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes exposing (style, class)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, (:=))
import Task
import Debug

-- MODEL

baseUrl = "http://swapi.co/api/people"

type alias Character =
  { name: String,
    height: String,
    mass: String,
    gender: String
  }


type alias Characters = List Character
type alias Next = String

type alias Model =
  { characters: Characters,
    next: Next
  }


newCharacter : String -> String -> String -> String -> Character
newCharacter name height mass gender =
  { name = name ,
    height = height,
    mass = mass,
    gender = gender
  }


init =
  ( Model [] baseUrl,
    fetchCharacters Nothing
    )

-- UPDATE

type Action
  = NoOp
  | LoadMore
  | ShowCharacters (Maybe Model)


-- TODO:
-- Keep track of current page in the model.
-- Update request to use the next page


update action model =
  case action of
    NoOp ->
      (model, Effects.none)

    LoadMore ->
      (model, fetchCharacters (Just model.next))

    ShowCharacters modelDef ->
      case modelDef of
        Just m ->
          ( Model (List.append model.characters m.characters) m.next
          , Effects.none)
        Nothing ->
          (model, Effects.none)

-- VIEW

view address model =
  div [ ]
    [ h2 [ ] [text "Star Wars App - Elm lang"]
    , viewCharacters model.characters
    , button [ class "load-more", onClick address LoadMore]
      [ text "Load More Characters" ]
    ]


characterView character =
    li [ class "characterView" ]
      [div [] [
          li [] [text character.name]
        , ul [] [
            li [] [text ("Name: " ++ character.name)]
          , li [] [text ("Mass (kg): " ++ character.mass)]
          , li [] [text ("Height (cm): " ++ character.height)]
          , li [] [text ("Gender: " ++ character.gender)]
          ]
      ]]


viewCharacters characters =
  ul [] (List.map characterView characters)


-- EFFECTS

fetchCharacters requestURL =
  Http.get results (Maybe.withDefault baseUrl requestURL)
    |> Task.toMaybe
    |> Task.map ShowCharacters
    |> Effects.task



decoder =
  Decode.object4 Character
    ("name" := Decode.string)
    ("height" := Decode.string)
    ("mass" := Decode.string)
    ("gender" := Decode.string)


results =
  Decode.object2 Model
    ("results" := Decode.list decoder)
    ("next" := Decode.string)
