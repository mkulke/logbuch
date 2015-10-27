import Html                exposing (Attribute, Html, h1, p, button, text, div, node)
import Html.Attributes     exposing (..)
import Html.Events         exposing (..)
import Signal              exposing (..)
import Time                exposing (..)
import Keyboard            exposing (..)
import Json.Decode as Json exposing (..)

-- MODEL

type alias Model =
    { counter: Int
    , hidden: Bool
    }

initialModel : Model
initialModel =
    { counter = 0
    , hidden = False
    }

type Action = Noop
            | Increment Int
            | Reset
            | Fetch

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

test : Int
test = 100

-- UPDATE

update : Action -> Model -> Model
update action model =
    case action of
        Increment n ->
            { model | counter <- model.counter + n }
        Reset ->
            { model | counter <- 0 }
        Fetch ->
            { model | counter <- test }

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
                  [ p [] [ text "omg" ]
                  , p [] [ text "rofl" ]
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
            , button [ class "btn btn-primary"
                     , onClick address Fetch
                     ]
                     [ text "Fetch" ]
            ]
        ]

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

counterStyle : Attribute
counterStyle =
  style
    [ ("width", "100%")
    , ("height", "40px")
    , ("padding", "10px 0")
    , ("font-size", "2em")
    , ("text-align", "center")
    ]

