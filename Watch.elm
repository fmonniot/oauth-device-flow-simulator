module Watch exposing (Model, Msg(..), State(..), TokenError(..), init, subscriptions, update, view)

{-| The Watch module is an implementation of the OAuth2 Device Flow
for a simulated watch (hence the module name).

It follows the Elm Architecture and exposes the basics function for it.
Thus most of the documentation will be focused on the state and messages more than
on the architecture itself.

Some type and function are exposed simply for the sake of this documentation. In
a real world project it shouldn't be exposed, but for this demo it's a good learning
experience :).


# State machine

@docs Model, State, Msg, TokenError


# Elm Architecture

@docs init, subscriptions, update, view

-}

import Html exposing (Html, br, button, div, h1, hr, input, label, li, program, span, text, ul)
import Html.Attributes exposing (checked, class, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode
import Time


-- MODEL


{-| State representation of the watch. The `Msg` below are the link between different state.

The watch is based on five state, plus one when an error occured. Each state can be considered
as a step in the flow. Here there are (in order):

  - `Welcome` is the first step. In this state a user is welcomed to the watch and asked if they
    wants to connect the watch. They can move to the next step by clicking the _connect_ button.

  - `Connecting` is a transiant state. We display this screen to let the user know their request
    have been made and to be patient :). If the response from the authorization server is
    successful then we pass to the next step, otherwise we display the `Error` state. In a real
    world application you may want to be a little more intelligent on how this error is managed
    and displayed to the user.

  - `WaitingAuthorization` is the next step. The user is displayed what to do next (going to the
    displayed URL and enter the given code). In the meantime the watch will poll the authorization
    server at the specified interval. In this demo, the user can change the polling interval and
    stop the polling itself. In a real world application this shouldn't be displaying to the user.
    At each call to the server, there is four possible outcomes:
    1.  The server respond with a valid token. In that case the application will move to the
        `Authorized` state.
    2.  The server respond with a _soft error_ (ie. slow down, authorization pending, ...).
        Those error won't change the current state. This demo app do nothing of them, in a real
        world application you should respond accordingly (eg. increase the polling interval if
        received a slow down error)
    3.  The server respond with a _hard errro_ `access_denied`. In that case the application will
        pass to the `Denied` state.
    4.  The server respond with any of the others _hard error_
        (ie. invalid_request, invalid_grant_type, ...). In that case we display the error message
        in the `Error` state. A real world application should be a bit more careful in the error
        message they display to the user.

  - `Authorized` is a final state, meaning the demo will stop here. It will show the obtained token.
    A real world application should display the connected version of itself here (that's why we
    wanted the whole flow after all ;)).

  - `Denied` is a final state. The user have denied access to the platform. In a real world application
    it could be interesting to explain to the user why your application needed the permissions
    they have been asked for alongside a "Connect again" button. In the end, the user still choose
    to deny access to your application, so maybe their are not interested again :(.

  - `Error` is a final state. It's a crude way of doing error management and shouldn't be done like
    that in a real world application. For the demo's sake, the simplicity is enough though.

-}
type State
    = Welcome
    | Connecting
    | WaitingAuthorization
    | Authorized
    | Denied
    | Error


{-| The model is the current state of the watch.

It contains the configuration, the current `State` and all the moving part
necessary to make the watch run. It is a good example of the minimum an
application will needs to implements the OAuth Device Flow (minus the polling
boolean which is not needed at all; unless you're doing a demo ;)).

Part of the Elm Architecture.

-}
type alias Model =
    { state : State
    , token : Maybe String
    , baseUrl : String
    , clientId : String
    , clientSecret : String
    , codes : Maybe Codes
    , error : Maybe String
    , pollingInterval : Int
    , polling : Bool
    }


defaultPolling : Int
defaultPolling =
    2


{-| Initial model.

Part of the Elm Architecture.

-}
init : Model
init =
    { state = Welcome
    , token = Nothing
    , baseUrl = "http://localhost:1337/accounts.artik.cloud:443"
    , clientId = "43252cf786e64ee59d6fb553eb08de4c"
    , clientSecret = "7147f1cfd9c74125b7ac5f5084cbd459"
    , codes = Nothing
    , error = Nothing
    , pollingInterval = defaultPolling
    , polling = True
    }



-- MESSAGES


{-| Messages can be grouped in two categories: the _state transition_ messages
and the _configuration_ messages. The former are responsible to pass from
a `State` to another while the latter let the user change the watch configuration at
runtime. Note that in a real world application, the _configuration_ messages should
not be made available to the end user.


## State Transition

  - `Connect` is fired when the user want to connect its watch. It will transition
    the state to `Connecting`.
  - `ReceivedCodes` is fired when the creation code complete. It contains the result
    of the HTTP call, either an `HTTP.Error`or the representation of the response.
  - `GetToken` is fired whenever a call to the token endpoint will be made, in the
    hope the application can have its token :).
  - `ReceivedAuthorization` is the result of a call to the `/token` endpoint. It
    contains either the token response (access token, refresh token, expiration time)
    or the error that happened.
  - `TryAgain` is a special case. It let the demo user begin a new flow while
    keeping the current logs as is. Useful when an error occured and retry.
    Hence the name :).


## Configuration

  - `UpdateClientId` let the demo user provide an OAuth client ID,
  - `UpdateClientSecret` let the demo user provide an OAuth client secret,
  - `UpdatePollingInterval` let the demo user overrides the polling interval,
  - `UpdateBaseUrl` let the demo user provide the authorization server base URL
    (the watch automatically append the path to it).
  - `TogglePolling` let the demo user stop the polling in the `WaitingAuthorization` state.
    This is mainly useful because the demo user may not want to pollute the logs
    with the same call falling again and again because the watch have not been
    authorized yet.

Part of the Elm Architecture

-}
type Msg
    = Connect
    | ReceivedCodes (Result Http.Error Codes)
    | GetToken
    | ReceivedAuthorization (Result TokenError TokenResponse)
    | TryAgain
    | UpdateClientId String
    | UpdateClientSecret String
    | UpdatePollingInterval Int
    | UpdateBaseUrl String
    | TogglePolling



-- UPDATE


{-| Update function.

It takes a message, the current model and compute what changes to apply.
For more information on what this function do, look up the `State` and `Msg` documentation.

Part of the Elm Architecture.

-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TryAgain ->
            { model | state = Welcome, token = Nothing, codes = Nothing, error = Nothing } ! []

        Connect ->
            { model | state = Connecting }
                ! [ fetchCodes model.baseUrl model.clientId ]

        ReceivedCodes (Result.Ok codes) ->
            { model | codes = Just codes, state = WaitingAuthorization, pollingInterval = codes.interval } ! []

        ReceivedCodes (Result.Err error) ->
            let
                msg =
                    case error of
                        Http.BadPayload _ _ ->
                            "BadPayload"

                        Http.BadStatus _ ->
                            "BadStatus"

                        _ ->
                            toString error
            in
            { model | error = Just msg, state = Error } ! []

        GetToken ->
            case model.codes of
                Just codes ->
                    model ! [ fetchToken model.baseUrl model.clientId model.clientSecret codes.deviceCode ]

                Nothing ->
                    model ! []

        ReceivedAuthorization (Result.Ok resp) ->
            { model | state = Authorized, token = Just resp.accessToken } ! []

        ReceivedAuthorization (Result.Err (SoftError _ _)) ->
            model ! []

        ReceivedAuthorization (Result.Err (HardError "access_denied" _ _)) ->
            { model | state = Denied } ! []

        ReceivedAuthorization (Result.Err (HardError error description _)) ->
            { model | error = Just error, state = Error } ! []

        UpdateClientId clientId ->
            { model | clientId = clientId } ! []

        UpdateClientSecret clientSecret ->
            { model | clientSecret = clientSecret } ! []

        UpdateBaseUrl baseUrl ->
            { model | baseUrl = baseUrl } ! []

        UpdatePollingInterval polling ->
            { model | pollingInterval = polling } ! []

        TogglePolling ->
            { model | polling = not model.polling } ! []



-- REST


type alias Codes =
    { deviceCode : String
    , userCode : String
    , verificationUrl : String
    , expiresIn : Int
    , interval : Int
    }


codesDecoder : Json.Decode.Decoder Codes
codesDecoder =
    Json.Decode.map5 Codes
        (Json.Decode.field "device_code" Json.Decode.string)
        (Json.Decode.field "user_code" Json.Decode.string)
        (Json.Decode.field "verification_url" Json.Decode.string)
        (Json.Decode.field "expires_in" Json.Decode.int)
        (Json.Decode.field "interval" Json.Decode.int)


fetchCodes : String -> String -> Cmd Msg
fetchCodes baseUrl clientId =
    let
        body =
            Http.stringBody "application/x-www-form-urlencoded" ("client_id=" ++ clientId)

        request =
            Http.post (baseUrl ++ "/device/code") body codesDecoder
    in
    Http.send ReceivedCodes request


type alias TokenResponse =
    { accessToken : String
    , refreshToken : String
    , expiresIn : Int
    , tokenType : String
    }


tokenResponseDecoder : Json.Decode.Decoder TokenResponse
tokenResponseDecoder =
    Json.Decode.map4 TokenResponse
        (Json.Decode.field "access_token" Json.Decode.string)
        (Json.Decode.field "refresh_token" Json.Decode.string)
        (Json.Decode.field "expires_in" Json.Decode.int)
        (Json.Decode.field "token_type" Json.Decode.string)


{-| TokenError represent an error returned by the authorization server on the `/token` endpoint.

Those errors can be divided in two categories:

  - `SoftError` are error on which the client can continue the flow.
    It contains the name of the error and an optional description of it.
    Most of the time, the description should be enough to understand what the problem is. If it's
    not clear enough, the different OAuth2 RFC can have the context of the error.
  - `HardError` are error which interrupt the flow. When one is found, the application
    will need to begin a new flow from the start. As with `SoftError`, it contains an error name
    and a possible description to help find what the problem is.

-}
type TokenError
    = SoftError String (Maybe String)
    | HardError String (Maybe String) Http.Error


tokenErrorFromHttpError : Http.Error -> TokenError
tokenErrorFromHttpError httpErr =
    case httpErr of
        Http.BadStatus resp ->
            case Json.Decode.decodeString tokenErrorDecoder resp.body of
                Result.Ok json ->
                    case json.error of
                        "authorization_pending" ->
                            SoftError json.error json.description

                        "slow_down" ->
                            SoftError json.error json.description

                        _ ->
                            HardError json.error json.description httpErr

                Result.Err jsonError ->
                    HardError "BadTokenError" (Just jsonError) httpErr

        Http.BadPayload reason resp ->
            HardError "BadPayload" (Just reason) httpErr

        _ ->
            HardError (toString httpErr) Nothing httpErr


type alias TokenErrorResponse =
    { error : String, description : Maybe String }


tokenErrorDecoder : Json.Decode.Decoder TokenErrorResponse
tokenErrorDecoder =
    Json.Decode.map2 TokenErrorResponse
        (Json.Decode.field "error" Json.Decode.string)
        (Json.Decode.field "error_description" (Json.Decode.maybe Json.Decode.string))


fetchToken : String -> String -> String -> String -> Cmd Msg
fetchToken baseUrl clientId clientSecret code =
    let
        body =
            Http.stringBody "application/x-www-form-urlencoded"
                ("grant_type=device_code"
                    ++ "&client_id="
                    ++ clientId
                    ++ "&client_secret="
                    ++ clientSecret
                    ++ "&code="
                    ++ code
                )

        request =
            Http.post (baseUrl ++ "/token") body tokenResponseDecoder

        cmd result =
            Result.mapError tokenErrorFromHttpError result |> ReceivedAuthorization
    in
    Http.send cmd request



-- VIEW


{-| Build the view representing the current state of the watch.

Part of the Elm Architecture.

-}
view : Model -> Html Msg
view model =
    let
        pollingUpdate s =
            UpdatePollingInterval (Result.withDefault defaultPolling (String.toInt s))

        view =
            case model.state of
                Welcome ->
                    welcomeView

                Connecting ->
                    connectingView

                WaitingAuthorization ->
                    case model.codes of
                        Just codes ->
                            waitingAuthorizationView codes

                        Nothing ->
                            errorView (Just "No codes received :(")

                Authorized ->
                    authorizedView

                Denied ->
                    deniedView

                Error ->
                    errorView model.error
    in
    div [ class "watch-container" ]
        [ div [ class "watch-frame" ] view
        , button [ onClick TryAgain ] [ text "Try Again" ]
        , hr [] []
        , config "Base URL:" (input [ placeholder "Base URL", onInput UpdateBaseUrl, value model.baseUrl ] [])
        , config "Client id:" (input [ placeholder "Client ID", onInput UpdateClientId, value model.clientId ] [])
        , config "Client secret:" (input [ placeholder "Client Secret", onInput UpdateClientSecret, value model.clientSecret ] [])
        , config "Polling interval:" (input [ placeholder "Polling Interval", onInput pollingUpdate, value (toString model.pollingInterval) ] [])
        , config "Polling" (input [ type_ "checkbox", onClick TogglePolling, checked model.polling ] [])
        ]


config : String -> Html Msg -> Html Msg
config name component =
    label [ class "watch-action" ] [ text name, component ]


welcomeView =
    [ div [ class "watch-text" ] [ text "Welcome to ARTIK Cloud" ]
    , div [ class "watch-bottom" ] [ button [ class "watch-connect", onClick Connect ] [ text "Connect" ] ]
    ]


connectingView =
    [ div [ class "watch-text" ] [ text "Connecting your device" ]
    , div [ class "watch-bottom" ] [ span [ class "watch-spinner" ] [ text "..." ] ]
    ]


waitingAuthorizationView : Codes -> List (Html Msg)
waitingAuthorizationView codes =
    [ div [ class "watch-text" ]
        [ text "Go to "
        , br [] []
        , text codes.verificationUrl
        , br [] []
        , text " and enter the code below to activate your device."
        ]
    , div [ class "watch-bottom" ] [ span [ class "watch-code" ] [ text codes.userCode ] ]
    ]


authorizedView =
    [ div [ class "watch-text" ] [ text "token = 7e1e6e68f42643e0b127ce0fa3c21fc8" ]
    , div [ class "watch-bottom" ] [ span [ class "watch-authorized" ] [ text "Connected !" ] ]
    ]


deniedView =
    [ div [ class "watch-text" ] [ text "You have denied access to your AKC account" ]
    , div [ class "watch-bottom" ] [ span [ class "watch-authorized" ] [ text "Not Connected :(" ] ]
    ]


errorView error =
    let
        errorMsg =
            case error of
                Just err ->
                    toString err

                Nothing ->
                    "No error"
    in
    [ div [ class "watch-text" ] [ text ("An error occured: " ++ errorMsg) ]
    , div [ class "watch-bottom" ] [ span [ class "watch-authorized" ] [ text "Details ->" ] ]
    ]



-- SUBSCRIPTIONS


{-| Subscriptions of this module.

when the state is in `WaitingAuthorization` and the `polling` boolean is true, then
we emit a `GetToken` message every `pollingInterval` seconds. Otherwise we do nothing.

Part of the Elm Architecture.

-}
subscriptions : Model -> Sub Msg
subscriptions model =
    if model.state == WaitingAuthorization && model.polling then
        Time.every (toFloat model.pollingInterval * Time.second) (always GetToken)
    else
        Sub.none
