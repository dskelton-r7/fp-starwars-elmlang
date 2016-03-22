module App where

import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes exposing (style, class, classList)
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
    next: Maybe Next
  }

newCharacter : String -> String -> String -> String -> Character
newCharacter name height mass gender =
  { name = name ,
    height = height,
    mass = mass,
    gender = gender
  }


init : (Model, Effects Action)
init =
  ( Model [] Nothing,
    fetchCharacters Nothing
    )


-- UPDATE


type Action
  = NoOp
  | LoadMore
  | ShowCharacters (Maybe Model)


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp ->
      (model, Effects.none)

    LoadMore ->
      case model.next of
        Just n -> (model, fetchCharacters model.next)
        Nothing -> (model, Effects.none)

    ShowCharacters modelDef ->
      case modelDef of
        Just m ->
          ( Model ((++) model.characters m.characters) m.next
          , Effects.none)
        Nothing ->
          (model, Effects.none)


-- VIEW


view : Signal.Address Action -> Model -> Html
view address model =
  div [ ]
    [ h2 [ ] [text "Star Wars App - Elm lang"]
    , viewCharacters model.characters
    , button [classList [("hidden", (model.next == Nothing))],
        onClick address LoadMore]
        [ text "Load More Characters" ]
    ]


characterView: Character -> Html
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


viewCharacters: Characters -> Html
viewCharacters characters =
  ul [] (List.map characterView characters)


-- EFFECTS


fetchCharacters : Maybe String -> Effects Action
fetchCharacters requestURL =
  Http.get results (Maybe.withDefault baseUrl requestURL)
    |> Task.toMaybe
    |> Task.map ShowCharacters
    |> Effects.task


decoder: Decoder Character
decoder =
  Decode.object4 Character
    ("name" := Decode.string)
    ("height" := Decode.string)
    ("mass" := Decode.string)
    ("gender" := Decode.string)


results: Decoder Model
results =
  Decode.object2 Model
    ("results" := Decode.list decoder)
    (Decode.maybe ("next" := Decode.string))
