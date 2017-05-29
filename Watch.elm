module Watch exposing (Model, Msg(..), State, init, subscriptions, update, view)

import Html exposing (Html, br, button, div, h1, hr, input, label, li, program, span, text, ul)
import Html.Attributes exposing (class, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode
import Time


-- MODEL


{-| State machine representation of the watch. The command indicates the possible state change.

    State | Message to next state | Next State
    Welcome -> Connect (Http Request) -> Connecting
    Connecting -> ReceivedCodes (Http response) -> WaitingAuthorization
    Connecting -> ReceivedCodes (Http response) -> Error

    WaitingAuthorization -> GetToken (Http Request) -> WaitingAuthorization

    WaitingAuthorization -> ReceivedAuthorization (HttpResponse) -> Authorized
    WaitingAuthorization -> ReceivedAuthorization (HttpResponse) -> Denied
    WaitingAuthorization -> ReceivedAuthorization (HttpResponse) -> WaitingAuthorization

-}
type State
    = Welcome
    | Connecting
    | WaitingAuthorization
    | Authorized
    | Denied
    | Error


type alias Model =
    { state : State
    , token : Maybe String
    , baseUrl : String
    , clientId : String
    , clientSecret : String
    , codes : Maybe Codes
    , error : Maybe String
    , polling : Int
    , poll : Bool
    }


defaultPolling : Int
defaultPolling =
    2


init : Model
init =
    { state = Welcome
    , token = Nothing
    , baseUrl = "http://localhost:1337/accounts-dev.artik.cloud:443"
    , clientId = "43252cf786e64ee59d6fb553eb08de4c"
    , clientSecret = ""
    , codes = Nothing
    , error = Nothing
    , polling = defaultPolling
    , poll = True
    }



-- MESSAGES


type Msg
    = Connect
    | ReceivedCodes (Result Http.Error Codes)
    | GetToken
    | ReceivedAuthorization (Result Http.Error TokenResponse)
    | TryAgain
    | UpdateClientId String
    | UpdateClientSecret String
    | UpdatePollingInterval Int
    | TogglePolling



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TryAgain ->
            { model | state = Welcome, token = Nothing, codes = Nothing, error = Nothing } ! []

        Connect ->
            { model | state = Connecting }
                ! [ fetchCodes model.baseUrl model.clientId ]

        ReceivedCodes result ->
            case result of
                Result.Ok codes ->
                    -- TODO Send UpdatePolling codes.interval
                    { model | codes = Just codes, state = WaitingAuthorization } ! []

                Result.Err error ->
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

        ReceivedAuthorization result ->
            model ! []

        UpdateClientId clientId ->
            { model | clientId = clientId } ! []

        UpdateClientSecret clientSecret ->
            { model | clientSecret = clientSecret } ! []

        UpdatePollingInterval polling ->
            { model | polling = polling } ! []

        TogglePolling ->
            { model | poll = not model.poll } ! []



-- REST


{-| {
"device_code": "4432df18-8ba4-4c14-816b-74e78a007eb2",
"user_code": "QUZD-PDAE",
"verification_url": "<https://artik.cloud/activate">,
"expires_in": 1800,
"interval": 5
}
-}
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


type alias TokenError =
    { error : String, description : Maybe String }


tokenErrorDecoder : Json.Decode.Decoder TokenError
tokenErrorDecoder =
    Json.Decode.map2 TokenError
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
    in
    Http.send ReceivedAuthorization request



-- VIEW


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
        , config "Client id:" (input [ placeholder "Client ID", onInput UpdateClientId, value model.clientId ] [])
        , config "Client secret:" (input [ placeholder "Client Secret", onInput UpdateClientSecret, value model.clientSecret ] [])
        , config "Polling interval:" (input [ placeholder "Polling Interval", onInput pollingUpdate, value (toString model.polling) ] [])
        , config "Stop polling" (input [ type_ "checkbox", onClick TogglePolling ] [])
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


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.state == WaitingAuthorization && model.poll then
        Time.every (toFloat model.polling * Time.second) (always GetToken)
    else
        Sub.none
