module Mgns where

import Html                exposing (Attribute, Html, h1,  h4, p, div, button, text, span)
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

port showModal : Signal Bool
port showModal =
  Signal.map (\m -> m.error /= NoError) app.model
    |> Signal.dropRepeats

type alias Model =
  { user       : User
  , isFetching : Bool
  , error      : Error
  }

type User = EmptyUser
          | FetchedUser String Int

type Error = NoError
           | UserError Int

init : (Model, Effects Action)
init =
  ( Model EmptyUser False NoError, Effects.none )

type Action = Noop
            | RequestName
            | NewName (Result Error User)

actions : Mailbox Action
actions =
  mailbox Noop

update : Action -> Model -> (Model, Effects Action)
update action model =
  let noToFetch =
    case model.error of
      NoError ->
        case model.user of
          EmptyUser -> 8
          FetchedUser string no -> no + 1
      UserError no -> no
  in
  case action of
    RequestName ->
      ( { model | isFetching <- True, error <- NoError }, fetchUsername noToFetch )
    NewName result ->
      case result of
        Err userError ->
          ( Model EmptyUser False userError, Effects.none )
        Ok fetchedUser ->
          ( Model fetchedUser False NoError, Effects.none )

dataDismiss : Attribute
dataDismiss =
  attribute "data-dismiss" "modal"

view : Signal.Address Action -> Model -> Html
view address model =
  let userText =
        case model.user of
          EmptyUser ->
            "Click fetch plz."
          FetchedUser name userNo ->
            toString userNo ++ ": " ++ name
      errorMsg =
        case model.error of
          UserError userNo ->
            "Could not fetch user no " ++ toString userNo
          NoError ->
            ""
      errorModal =
        div [ id "error-modal", class "modal fade in" ]
          [ div [ class "modal-dialog modal-sm" ]
            [ div [ class "modal-content" ]
              [ div [ class "modal-header" ]
                [ button [ dataDismiss, class "close" ] [ span [] [ text "×" ] ]
                , h4 [ class "modal-title" ] [ text "Error" ]
                ]
              , div [ class "modal-body" ] [ p []
                  [ text errorMsg ]
                ]
              ]
            ]
          ]
  in
  div []
    [ div [ class "container" ]
      [ div [ class "row" ] [ h1 [] [ text "Mgns" ] ]
      , div [ class "row" ]
        [ p [] [ text userText ]
        , button [ onClick address RequestName, disabled model.isFetching
                 , classList [ ("btn", True)
                             , ("btn-primary", True)
                             ]
                 ] [ text "Fetch" ]
        ]
      ]
    , div [] [ errorModal ]
    ]

fetchUsername : Int -> Effects Action
fetchUsername userNo =
  let url = "http://jsonplaceholder.typicode.com/users/" ++ (toString userNo)
  in Http.get decodeName url
    |> Task.map (\string -> FetchedUser string userNo)
    |> Task.mapError (\e -> UserError userNo)
    |> Task.toResult
    |> Task.map NewName
    |> Effects.task

decodeName : Json.Decoder (String)
decodeName =
  "name" := Json.string
