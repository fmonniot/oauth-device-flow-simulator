module Main exposing (..)

import Date
import Dict
import Dom.Scroll
import Html exposing (Html, button, div, h1, input, li, program, span, text, ul)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Http
import Log
import Messages exposing (..)
import Task
import Tuple
import Watch


-- MODEL


type alias Model =
    { watch : Watch.Model
    , logs : List Log.Model
    }


init : ( Model, Cmd Msg )
init =
    ( { watch = Watch.init
      , logs = []
      }
    , Task.perform (\_ -> DisplayConfig) (Task.succeed 42)
    )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "wrapper" ]
        [ topBar
        , Html.map WatchMsg (Watch.view model.watch)
        , Log.view model.logs
        , Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "styles.css" ] []
        ]


topBar : Html Msg
topBar =
    div [ class "header" ]
        [ h1 [] [ text "Device Flow Simulator" ]
        , ul [] [ li [] [ button [ onClick ResetAll ] [ text "Reset all" ] ] ]
        ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update outerMsg model =
    case outerMsg of
        NoOp ->
            model ! []

        WithTime date msg ->
            let
                logs =
                    logsOf msg date model

                scrollToBottom =
                    Task.attempt (\_ -> NoOp) (Dom.Scroll.toBottom "log-container")
            in
            case msg of
                NoOp ->
                    model ! []

                WatchMsg watchMsg ->
                    let
                        ( updatedWatch, cmd ) =
                            Watch.update watchMsg model.watch
                    in
                    { model | watch = updatedWatch, logs = logs }
                        ! [ Cmd.map WatchMsg cmd, scrollToBottom ]

                ResetAll ->
                    Tuple.mapFirst (\m -> { m | logs = logs }) init

                DisplayConfig ->
                    { model | logs = logs } ! [ scrollToBottom ]

                WithTime _ _ ->
                    { model | logs = logs } ! [ scrollToBottom ]

        msg ->
            ( model, Task.perform (\d -> WithTime d msg) Date.now )


{-| logsOf will return the list of log statement associated with a given message.
-}
logsOf : Msg -> Date.Date -> Model -> List Log.Model
logsOf msg date model =
    case msg of
        WatchMsg watchMsg ->
            let
                logs =
                    case watchMsg of
                        Watch.TryAgain ->
                            [ Log.warn date "Beginning new Device Flow" [] ]

                        Watch.Connect ->
                            [ Log.info date "User clicked connect" []
                            , Log.info date
                                "Making request to ARTIK Cloud"
                                [ Log.Http "POST" (model.watch.baseUrl ++ "/device/code")
                                , Log.Header "Content-Type" "application/json"
                                , Log.Data (Log.record [ Log.recordValue "client_id" model.watch.clientId ])
                                ]
                            ]

                        Watch.ReceivedCodes result ->
                            case result of
                                Result.Ok codes ->
                                    let
                                        record =
                                            Log.record
                                                [ Log.recordValue "device_code" codes.deviceCode
                                                , Log.recordValue "user_code" codes.userCode
                                                , Log.recordValue "verification_url" codes.verificationUrl
                                                , Log.recordValue "expires_in" (toString codes.expiresIn)
                                                , Log.recordValue "interval" (toString codes.interval)
                                                ]
                                    in
                                    [ Log.info date
                                        "Received code response from AKC"
                                        [ Log.Data record
                                        , Log.Status 200
                                        , Log.Header "Accept" "application/json, */*"
                                        ]
                                    , Log.warn date ("Setting up the polling interval to " ++ toString codes.interval ++ "sec") []
                                    ]

                                Result.Err httpError ->
                                    [ logHttpError date httpError ]

                        Watch.GetToken ->
                            let
                                code =
                                    case model.watch.codes of
                                        Just codes ->
                                            codes.deviceCode

                                        Nothing ->
                                            "No code available"

                                record =
                                    Log.record
                                        [ Log.recordValue "client_id" model.watch.clientId
                                        , Log.recordValue "client_secret" model.watch.clientSecret
                                        , Log.recordValue "code" code
                                        , Log.recordValue "grant_type" "device_code"
                                        ]
                            in
                            [ Log.info date "Device polling ARTIK Cloud for a token" []
                            , Log.info date
                                "Making request to ARTIK Cloud"
                                [ Log.Http "POST" (model.watch.baseUrl ++ "/token")
                                , Log.Header "Content-Type" "application/x-www-form-urlencoded"
                                , Log.Data record
                                ]
                            ]

                        Watch.ReceivedAuthorization result ->
                            case result of
                                Result.Ok resp ->
                                    let
                                        record =
                                            Log.record
                                                [ Log.recordValue "access_token" resp.accessToken
                                                , Log.recordValue "refresh_token" resp.refreshToken
                                                , Log.recordValue "expires_in" (toString resp.expiresIn)
                                                , Log.recordValue "token_type" resp.tokenType
                                                ]
                                    in
                                    [ Log.info date
                                        "Received code response from AKC"
                                        [ Log.Data record
                                        , Log.Status 200
                                        , Log.Header "Accept" "application/json, */*"
                                        ]
                                    ]

                                Result.Err (Watch.SoftError name desc) ->
                                    let
                                        msg =
                                            Maybe.map (\m -> "; description: `" ++ m ++ "`") desc |> Maybe.withDefault ""
                                    in
                                    [ Log.info date ("Soft Error `" ++ name ++ "`" ++ msg) [] ]

                                Result.Err (Watch.HardError name desc httpErr) ->
                                    let
                                        msg =
                                            Maybe.map (\m -> "; description: `" ++ m ++ "`") desc |> Maybe.withDefault ""
                                    in
                                    [ Log.error date ("Hard Error `" ++ name ++ "`" ++ msg) []
                                    , logHttpError date httpErr
                                    ]

                        Watch.UpdateClientId newClientId ->
                            let
                                oldClientId =
                                    model.watch.clientId
                            in
                            [ Log.warn date "Updating the client id" [ Log.ChangeValue oldClientId newClientId ] ]

                        Watch.UpdateClientSecret newClientSecret ->
                            let
                                oldClientSecret =
                                    model.watch.clientSecret
                            in
                            [ Log.warn date "Updating the client secret" [ Log.ChangeValue oldClientSecret newClientSecret ] ]

                        Watch.UpdatePollingInterval newPollingInterval ->
                            let
                                oldPollingInterval =
                                    model.watch.pollingInterval

                                toS i =
                                    toString i ++ "sec"

                                details =
                                    [ Log.ChangeValue (toS oldPollingInterval) (toS newPollingInterval) ]
                            in
                            [ Log.warn date "Updating the polling interval" details ]

                        Watch.TogglePolling ->
                            let
                                message =
                                    if model.watch.polling then
                                        "Stopping AKC polling"
                                    else
                                        "Will poll AKC each " ++ toString model.watch.polling ++ " seconds"
                            in
                            [ Log.warn date message [] ]
            in
            model.logs ++ logs

        NoOp ->
            []

        ResetAll ->
            [ Log.warn date "Configuration reset" [] ]

        DisplayConfig ->
            let
                polling =
                    toString model.watch.pollingInterval ++ "sec"

                details =
                    Log.Data
                        (Log.record
                            [ Log.recordValue "client_id" model.watch.clientId
                            , Log.recordValue "client_secret" model.watch.clientSecret
                            , Log.recordValue "polling" polling
                            ]
                        )
            in
            model.logs ++ [ Log.info date "Current configuration:" [ details ] ]

        WithTime _ _ ->
            model.logs ++ [ Log.info date "Received WithTime message wrapped two times !" [] ]


logHttpError : Date.Date -> Http.Error -> Log.Model
logHttpError date err =
    case err of
        Http.BadUrl url ->
            Log.info date
                "The provided URL was not valid"
                [ Log.Header "URL" url
                , Log.Header "Error" "BadUrl"
                ]

        Http.Timeout ->
            Log.info date
                "Requested endpoint timed out"
                [ Log.Header "Error" "Timeout"
                ]

        Http.NetworkError ->
            Log.info date
                "An unknown error occured. Maybe you are disconnected ?"
                [ Log.Header "Error" "NetworkError"
                ]

        Http.BadStatus res ->
            let
                response =
                    logResponse res
            in
            Log.info date
                "Received a response from AKC with a bad status code"
                ([ Log.Header "Error" "BadStatus" ] ++ response)

        Http.BadPayload why res ->
            Log.info date
                "Malformed response from AKC"
                ([ Log.Header "Error" "BadPayload"
                 , Log.Header "Reason" why
                 ]
                    ++ logResponse res
                )


logResponse : Http.Response String -> List Log.KeyValue
logResponse res =
    [ Log.Header "URL" res.url
    , Log.Status res.status.code
    , Log.Header "Status message" res.status.message
    , Log.Header "Body" res.body
    ]
        ++ (Dict.toList res.headers |> List.map (\( a, b ) -> Log.Header a b))



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.none
        , Sub.map WatchMsg (Watch.subscriptions model.watch)
        ]



-- MAIN


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
