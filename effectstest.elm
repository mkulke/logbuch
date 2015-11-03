import Html                exposing (Attribute, Html, p, div, button, text)
import Html.Events         exposing (..)
import Html.Attributes     exposing (..)
import Signal              exposing (..)
import Http                exposing (..)
import Json.Decode as Json exposing ((:=))
import Task                exposing (Task, andThen, onError, succeed)
import Effects             exposing (Effects, Never)
import StartApp

app = StartApp.start
  { init   = init
  , update = update
  , view   = view
  , inputs = []
  }

main =
  app.html

port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks

type alias Model =
  { name       : String
  , isFetching : Bool
  , userNo     : Int
  }

init : (Model, Effects Action)
init =
  ( Model "default" False 0, Effects.none )

type Action = Noop
            | RequestName
            | NewName Int (Maybe String)

actions : Mailbox Action
actions =
  mailbox Noop

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    RequestName ->
      ({ model | isFetching <- True }, fetchUsername (model.userNo + 1))
    NewName userNo maybeName ->
      (Model (Maybe.withDefault "Oh my!" maybeName) False userNo, Effects.none)

view : Signal.Address Action -> Model -> Html
view address model =
  div [ style [ ("padding", "20px") ] ]
    [ p [] [ text (toString model.userNo ++ ": " ++ model.name) ]
    , button [ onClick address RequestName, disabled model.isFetching ] [ text "Fetch" ]
    ]

fetchUsername : Int -> Effects Action
fetchUsername userNo =
  let url = "http://jsonplaceholder.typicode.com/users/" ++ (toString userNo)
  in Http.get decodeName url
    |> Task.toMaybe
    |> Task.map (NewName userNo)
    |> Effects.task

decodeName : Json.Decoder (String)
decodeName =
  "name" := Json.string
