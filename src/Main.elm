port module Main exposing (main)

import Browser exposing (Document)
import Html exposing (Html, button, div, input, text)
import Html.Events
import Json.Encode exposing (Value)


type alias Model =
    { targetName : List String
    , args : List Json.Encode.Value
    }


type Msg
    = TargetNameChanged String
    | ArgChanged String
    | RunButtonClicked


port runForeign : ( List String, List Json.Encode.Value ) -> Cmd msg


main =
    Browser.document
        { init =
            \() ->
                ( { targetName = []
                  , args = []
                  }
                , Cmd.none
                )
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


view : Model -> Document Msg
view model =
    { title = "Ports Playground"
    , body = [ body model ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TargetNameChanged string ->
            ( { model | targetName = String.split "." string }, Cmd.none )

        ArgChanged string ->
            ( { model | args = [ Json.Encode.string string ] }, Cmd.none )

        RunButtonClicked ->
            ( model, runForeign ( model.targetName, model.args ) )


body : Model -> Html Msg
body model =
    div []
        [ input [ Html.Events.onInput TargetNameChanged ] [ text "target" ]
        , input [ Html.Events.onInput ArgChanged ] [ text "arg" ]
        , button [ Html.Events.onClick RunButtonClicked ] [ text "Run" ]
        ]
