import Html                exposing (Attribute, Html, h1, b, p, button, text, div, node)
import Html.Attributes     exposing (..)
import Html.Events         exposing (..)
import Signal              exposing (..)
import Time                exposing (..)
import Keyboard            exposing (..)
import Http                exposing (..)
import Json.Decode as Json exposing ((:=))
import Task                exposing (Task, andThen, onError, succeed)
import TaskTutorial        exposing (print, getCurrentTime)

-- MODEL

type alias Model =
  { counter  : Int
  , fetching : Bool
  , name     : String
  }

initialModel : Model
initialModel =
  { counter  = 0
  , fetching = False
  , name     = "default"
  }

type Action = Noop
            | Increment Int
            | Reset
            | Fetch
            | SetText String

actions : Mailbox Action
actions =
  mailbox Noop

keyboard : Signal Action
keyboard =
  Signal.map
  (\t -> Increment 10)
  Keyboard.presses

timer : Signal Action
timer =
  Signal.map
  (\t -> Increment 1)
  (every second)

model : Signal Model
model =
  Signal.foldp update initialModel
  (Signal.mergeMany [actions.signal, timer, keyboard])

results : Signal.Mailbox String
results =
  Signal.mailbox "default"

test : Int
test = 100

decodeTest : Json.Decoder (String)
decodeTest =
  "name" := Json.string

report : String -> Task x ()
report name =
  Signal.send actions.address (SetText name)

port fetchTest : Task Http.Error ()
port fetchTest =
  andThen
    (Http.get decodeTest "http://jsonplaceholder.typicode.com/users/1")
    report

-- safeGet : Task (List String)
-- safeGet =
--   get `onError` (\err -> succeed [])

-- port runner : Signal (Task x ())
-- port runner =
--   Signal.map (\t -> Task.toResult t `andThen` print)(Signal.map get (Signal.filter (\a -> a == Fetch) Noop actions.signal))

-- UPDATE

update : Action -> Model -> Model
update action model =
  case action of
    Increment n ->
      { model | counter <- model.counter + n }
    Reset ->
      { model | counter <- 0 }
    Fetch ->
      { model | fetching <- True }
    SetText s ->
      { model | name <- s }

-- MAIN

main : Signal Html
main =
  Signal.map (view actions.address) model

-- VIEW

view : Address Action -> Model -> Html
view address model =
  div []
    [ includeBootstrap
    , container
      [ h1 [] [ text (toString model.counter) ]
      , div [ id "collapseExample", class "collapse in" ]
        [ div [ class "well" ]
          [ p []
            [ b []
              [ text "Name:" ]
            ]
          , p [] [ text model.name ]
          ]
        ]
      , button [ class "btn btn-default", onClick address Reset ] [ text "Reset" ]
      , text " "
      , button [ class "btn btn-default"
               , dataToggle "collapse"
               , dataTarget "collapseExample"
               ]
               [ text "Hide" ]
      , text " "
               , button [ class ("btn btn-primary" ++ disabled model.fetching)
               , onClick address Fetch
               ]
               [ text "Fetch" ]
      ]
    ]

disabled : Bool -> String
disabled bool =
  if bool then " disabled" else ""

dataTarget : String -> Attribute
dataTarget text =
  attribute "data-target" ("#" ++ text)

dataToggle : String -> Attribute
dataToggle text =
  attribute "data-toggle" text

container : List Html -> Html
container children =
  div [ class "container" ] children

includeBootstrap : Html
includeBootstrap =
  div []
    [ node "link"
      [ rel "stylesheet"
      , href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
      ]
      []
    , node "script" [ src "https://ajax.googleapis.com/ajax/libs/jquery/2.1.4/jquery.min.js" ] []
    , node "script" [ src "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js" ] []
    ]
