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

type alias Character =
  { name: String,
    height: String,
    mass: String,
    gender: String
  }


type alias Model =
  { characters: List Character
  }


newCharacter : String -> String -> String -> String -> Character
newCharacter name height mass gender =
  { name = name ,
    height = height,
    mass = mass,
    gender = gender
  }


init page =
  ( Model [],
    fetchCharacters page
    )

-- UPDATE

type Action
  = NoOp
  | LoadMore
  | ShowCharacters (Maybe(List(Character)))


update action model =
  case action of
    NoOp ->
      (model, Effects.none)

    LoadMore ->
      (model, fetchCharacters "2")

    ShowCharacters maybeCharacters ->
      let
        characters =
          List.append
            model.characters
            (Maybe.withDefault model.characters maybeCharacters)
      in
        (Model characters, Effects.none)


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

fetchCharacters page =
  Http.get results (charactersUrl page)
    |> Task.toMaybe
    |> Task.map ShowCharacters
    |> Debug.log "characters::"
    |> Effects.task


charactersUrl page =
  Http.url "http://swapi.co/api/people/" [ ("page", page) ]

decoder =
  Decode.object4 Character
    ("name" := Decode.string)
    ("height" := Decode.string)
    ("mass" := Decode.string)
    ("gender" := Decode.string)


results =
  Decode.object1 identity
    ("results" := Decode.list decoder)
