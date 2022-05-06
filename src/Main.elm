port module Main exposing (main)

import Browser exposing (Document)
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (class)
import Html.Events
import Json.Decode as JD
import Json.Encode as JE exposing (Value)


type alias Model =
    { targetName : List String
    , args : List JE.Value
    , commandCounter : Int
    , results : List (FfiResult JD.Value)
    }


type Msg
    = TargetNameChanged String
    | ArgChanged String
    | RunButtonClicked
    | ListenEventClicked
    | FunctionInvoked (FfiResult JD.Value)


type alias FfiId =
    String


type ForeignFunctionInvocation
    = Call { id : FfiId, cmd : List String, args : List JE.Value }
    | New { id : FfiId, cmd : List String, args : List JE.Value }
    | Create { id : FfiId, cmd : List String, args : List JE.Value }
    | InvokeOn { target : FfiId, id : FfiId, cmd : List String, args : List JE.Value }
    | ListenOn { target : FfiId, id : FfiId, cmd : List String, args : List JE.Value }


encodeInvocation : ForeignFunctionInvocation -> JE.Value
encodeInvocation foreignFunctionInvocation =
    case foreignFunctionInvocation of
        Call record ->
            JE.object <| ( "__type", JE.string "Call" ) :: encodeCall record

        New record ->
            JE.object <| ( "__type", JE.string "New" ) :: encodeCall record

        Create record ->
            JE.object <| ( "__type", JE.string "Create" ) :: encodeCall record

        InvokeOn record ->
            JE.object <| ( "__type", JE.string "InvokeOn" ) :: encodeMethodCall record

        ListenOn record ->
            JE.object <| ( "__type", JE.string "ListenOn" ) :: encodeMethodCall record


encodeCall : { id : FfiId, cmd : List String, args : List Value } -> List ( String, Value )
encodeCall record =
    [ ( "id", JE.string record.id )
    , ( "cmd", JE.list JE.string record.cmd )
    , ( "args", JE.list identity record.args )
    ]


encodeMethodCall : { target : FfiId, id : FfiId, cmd : List String, args : List Value } -> List ( String, Value )
encodeMethodCall record =
    [ ( "target", JE.string record.target )
    , ( "id", JE.string record.id )
    , ( "cmd", JE.list JE.string record.cmd )
    , ( "args", JE.list identity record.args )
    ]


runForeign : ForeignFunctionInvocation -> Cmd msg
runForeign ffi =
    runForeign_ (encodeInvocation ffi)


{-| run a javascript function for it's side effects.
id can be used to listen to whether the function was called
cmd is a path identifier for the function from the Window object.
args is a list of any serializable values, given to the function as arguments.
-}
port runForeign_ : JE.Value -> Cmd msg


{-| Listen to results from foreign function invocations
JS side should return one of:

  - `[ id, { __type: "Ok", value: v } ]`

  - `[ id, { __type: "NotFound", cmd: array } ]`

  - `[ id, { __type: "Exception", "exception": msg, cmd: array, args: args } ]`

    where id is a correlation ID used when requesting invocation
    v is a value that needs to be decoded by the user
    msg is the exception message

-}
port foreignResult_ : (( FfiId, JD.Value ) -> msg) -> Sub msg


type FfiResult a
    = Success FfiId a
    | SuccessNoValue FfiId
    | NotFound FfiId (List String)
    | ExceptionThrown FfiId { message : String, cmd : List String, args : List JE.Value }
    | DecodingFailed FfiId JD.Error


toDebugString : (a -> String) -> FfiResult a -> String
toDebugString aToStr res =
    case res of
        Success ffiId a ->
            "Success (" ++ ffiId ++ "): " ++ aToStr a

        SuccessNoValue ffiId ->
            "Success (" ++ ffiId ++ ") <void>"

        NotFound ffiId strings ->
            "NotFound (" ++ ffiId ++ "): " ++ String.join "." strings

        ExceptionThrown ffiId { message, cmd, args } ->
            "ExceptionThrown ("
                ++ ffiId
                ++ "): "
                ++ message
                ++ "\n in Invocation: "
                ++ String.join "." cmd
                ++ "("
                ++ (args |> List.map (JE.encode 0) |> String.join ", ")
                ++ ")"

        DecodingFailed ffiId error ->
            "DecodingFailed (" ++ ffiId ++ "): " ++ JD.errorToString error


foreignResult : JD.Decoder a -> (FfiResult a -> msg) -> Sub msg
foreignResult decoder toMessage =
    let
        resultDecoder : FfiId -> JD.Decoder (FfiResult a)
        resultDecoder id =
            JD.field "__type" JD.string
                |> JD.andThen
                    (\type_ ->
                        case type_ of
                            "Ok" ->
                                JD.field "value" (JD.nullable decoder)
                                    |> JD.andThen
                                        (\v ->
                                            case v of
                                                Just some ->
                                                    JD.succeed (Success id some)

                                                Nothing ->
                                                    JD.succeed (SuccessNoValue id)
                                        )

                            "NotFound" ->
                                JD.field "cmd" (JD.list JD.string)
                                    |> JD.andThen
                                        (\cmd ->
                                            JD.succeed (NotFound id cmd)
                                        )

                            "Exception" ->
                                JD.map3 (\exc cmd args -> ExceptionThrown id { message = exc, cmd = cmd, args = args })
                                    (JD.field "exception" JD.string)
                                    (JD.field "cmd" (JD.list JD.string))
                                    (JD.field "args" (JD.list JD.value))

                            _ ->
                                JD.fail ("Unknown __type '" ++ type_ ++ "'")
                    )

        decodeResult : FfiId -> JD.Value -> FfiResult a
        decodeResult id value =
            let
                result =
                    JD.decodeValue (resultDecoder id) value
            in
            case result of
                Ok decoded ->
                    decoded

                Err jdErr ->
                    DecodingFailed id jdErr
    in
    foreignResult_
        (\( id, value ) ->
            decodeResult id value |> toMessage
        )


main =
    Browser.document
        { init =
            \() ->
                ( { targetName = []
                  , args = []
                  , commandCounter = 0
                  , results = []
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
    foreignResult JD.value FunctionInvoked


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TargetNameChanged string ->
            ( { model | targetName = String.split "." string }, Cmd.none )

        ArgChanged string ->
            ( { model | args = [ JE.string string ] }, Cmd.none )

        RunButtonClicked ->
            ( { model | commandCounter = model.commandCounter + 1 }
            , runForeign
                (Call
                    { id = String.join "." model.targetName ++ "#" ++ String.fromInt model.commandCounter
                    , cmd = model.targetName
                    , args = model.args
                    }
                )
            )

        FunctionInvoked ffiResult ->
            ( { model | results = ffiResult :: model.results }, Cmd.none )

        ListenEventClicked ->
            ( { model | commandCounter = model.commandCounter + 1 }
            , runForeign
                (ListenOn
                    { id = String.join "." model.targetName ++ "#listenOn-" ++ String.fromInt model.commandCounter
                    , cmd = model.targetName
                    , args = model.args
                    , target = ""
                    }
                )
            )


body : Model -> Html Msg
body model =
    div [ class "col" ]
        [ div [ class "row" ]
            [ input [ Html.Events.onInput TargetNameChanged, Html.Attributes.value (String.join "." model.targetName) ] [ text "target" ]
            , input [ Html.Events.onInput ArgChanged ] [ text "arg" ]
            , button [ Html.Events.onClick RunButtonClicked ] [ text "Call Global" ]
            , button [ Html.Events.onClick ListenEventClicked ] [ text "Listen To Event" ]
            ]
        , model.results
            |> List.map (toDebugString (JE.encode 0))
            |> List.map (\s -> div [ class "result" ] [ text s ])
            |> div [ class "col" ]
        ]
