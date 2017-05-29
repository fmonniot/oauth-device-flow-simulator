module Log exposing (KeyValue(..), Model, info, record, recordChildren, recordValue, view, warn)

import Date
import Html exposing (Html, br, button, div, h1, input, li, program, span, text, ul)
import Html.Attributes exposing (class, placeholder)
import Messages exposing (Msg)


type Record
    = One String String -- name, value
    | OneToMany String (List Record) -- name, children
    | ManyToMany (List Record)


record : List Record -> Record
record children =
    ManyToMany children


recordChildren : String -> List Record -> Record
recordChildren name children =
    OneToMany name children


recordValue : String -> String -> Record
recordValue name value =
    One name value


type KeyValue
    = ChangeValue String String
    | Status Int
    | Http String String
    | Header String String
    | Data Record
    | NoData


type Level
    = Info
    | Warn


type alias Model =
    { lvl : Level, date : Date.Date, message : String, details : List KeyValue }


info : Date.Date -> String -> List KeyValue -> Model
info date message details =
    Model Info date message details


warn : Date.Date -> String -> List KeyValue -> Model
warn date message details =
    Model Warn date message details


view : List Model -> Html Messages.Msg
view logs =
    let
        entries =
            List.map logEntry logs
    in
    div [ class "log-container" ] entries


logEntry : Model -> Html Msg
logEntry log =
    let
        date =
            "[" ++ dateToString log.date ++ "]"

        messageClass =
            case log.lvl of
                Info ->
                    "info"

                Warn ->
                    "warn"

        details d =
            case d of
                Http method path ->
                    div [ class "log-details" ]
                        [ div []
                            [ span [ class "http-method" ] [ text method ]
                            , span [ class "http-path" ] [ text path ]
                            ]
                        ]

                Header name value ->
                    div [ class "log-details" ] [ header name value False ]

                ChangeValue old new ->
                    div [ class "log-details" ]
                        [ span [ class "change-value-old" ] [ text old ]
                        , span [ class "change-value-new" ] [ text new ]
                        ]

                Status status ->
                    div [ class "log-details" ]
                        [ div []
                            [ span [ class "http-status-code" ] [ text (toString status) ]
                            , span [ class "http-status-text" ] [ text (statusAsText status) ]
                            ]
                        ]

                Data record ->
                    div [ class "log-details", class "record" ] (recordView 0 record)

                NoData ->
                    div [ class "log-details" ] [ header "Body" "none" True ]
    in
    div [ class "log-entry" ]
        ([ span [ class "date" ] [ text date ]
         , span [ class messageClass ] [ text log.message ]
         ]
            ++ (log.details |> List.sortBy sortByKeyValue |> List.map details)
        )


sortByKeyValue : KeyValue -> String
sortByKeyValue detail =
    case detail of
        Http method path ->
            "B" ++ method ++ path

        Header name value ->
            let
                prefix =
                    if name == "Body" then
                        "E"
                    else
                        "D"
            in
            prefix ++ name ++ value

        ChangeValue old new ->
            "A" ++ new ++ old

        Status status ->
            "C" ++ toString status

        Data record ->
            "E" ++ toString record

        NoData ->
            "E"


recordView : Int -> Record -> List (Html Msg)
recordView indent record =
    case record of
        One name value ->
            let
                n =
                    "\"" ++ name ++ "\""

                v =
                    "\"" ++ value ++ "\""
            in
            [ div [ class "indent" ]
                [ span [ class "identifier" ] [ text n ]
                , text ": "
                , span [ class "value" ] [ text v ]
                ]
            ]

        OneToMany name children ->
            let
                identifier =
                    span [ class "identifier" ] [ text ("\"" ++ name ++ "\":") ]
            in
            [ div [ class "indent" ]
                ([ identifier, text " {", br [] [] ]
                    ++ List.concatMap (recordView (indent + 1)) children
                    ++ [ span [] [ text "}" ] ]
                )
            ]

        ManyToMany children ->
            [ text "{", br [] [] ]
                ++ List.concatMap (recordView (indent + 1)) children
                ++ [ text "}" ]


dateToString : Date.Date -> String
dateToString date =
    let
        toS i =
            if i > 9 then
                toString i
            else
                "0" ++ toString i

        str =
            [ Date.hour date, Date.minute date, Date.second date ]
                |> List.map toS
                |> String.join ":"
    in
    str ++ "." ++ toString (Date.millisecond date)


header : String -> String -> Bool -> Html Msg
header name value emphasis =
    let
        classes =
            [ class "http-header-value" ]
                ++ (if emphasis then
                        [ class "italic" ]
                    else
                        []
                   )
    in
    div []
        [ span [ class "http-header-name" ] [ text name ]
        , span classes [ text value ]
        ]


statusAsText : Int -> String
statusAsText status =
    case status of
        200 ->
            "OK"

        401 ->
            "Unauthorized"

        _ ->
            "Unknown"
