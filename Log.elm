module Log exposing (KeyValue(..), Log, error, info, record, recordChildren, recordValue, view, warn)

{-| This module expose a way to display a console-like interface.

A console is, at its base, just a list of log. This module give you way to construct those logs and
to display them in a friendly way.


# Log

@docs Log
@docs info, warn, error


# Details

@docs KeyValue
@docs record, recordChildren, recordValue


# Elm Architecture

@docs view

-}

import Date
import Html exposing (Html, br, button, div, h1, input, li, program, span, text, ul)
import Html.Attributes exposing (class, id, placeholder)
import Messages exposing (Msg)


type Record
    = One String String -- name, value
    | OneToMany String (List Record) -- name, children
    | ManyToMany (List Record) -- children only


{-| A `Record` can be viewed as a JSON element. An example will let you understand really quickly
how to use and build records:

    record
        [ recordValue "device_code" codes.deviceCode
        , recordValue "user_code" codes.userCode
        , recordValue "verification_url" codes.verificationUrl
        , recordValue "expires_in" (toString codes.expiresIn)
        , recordValue "interval" (toString codes.interval)
        ]

could be translated into the following JSON

    {
        "device_code": "value",
        "user_code": "value",
        "verification_url": "value",
        "expires_in": "value",
        "interval": "value"
    }

To build a `Record`, you have access to this function which can be used to build
_root_ element of a record and `recordChildren` and `recordValue`, which build named
object and simple key value respectively.

-}
record : List Record -> Record
record children =
    ManyToMany children


{-| This function let you build a named record containing other record.

    recordChilden "parent"
        [ recordValue "k1" "v1
        , recordValue "k2" "v2"
        ]

would be translated into

    {
        "parent": {
            "k1": "v1",
            "k2": "v2"
        }
    }

-}
recordChildren : String -> List Record -> Record
recordChildren name children =
    OneToMany name children


{-| A simple key value record
-}
recordValue : String -> String -> Record
recordValue name value =
    One name value


{-| A KeyValue is a way to pass more information to a `Log` statement.

  - `ChangeValue` is obvious: it indicates a value which have changed (eg. the user changed a configuration)
  - `Status` indicates a HTTP status. The log will print the numeric value alongside the associated english name.
  - `Http` represents the basis of a HTTP request/response. The first string is the method and the second is the path.
  - `Header` represents a HTTP header, with the first field being the name and the second the value
  - `Data` represents a structured data which will be displayed as JSON. More information on the `Record` type below.
  - `NoData` indicates the `Body` of the request/response is empty.

-}
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
    | Error


{-| A `Log` is a dated piece of information with optional details (metadata) associated.

A `Log` can have associated details to it. They are represented by a `List` of key value.
More information in the `KeyValue` type.

To build a `Log` use the three method `info`, `warn` or `error` below, depending on the
importance you want to give to the information.

-}
type Log
    = Log { lvl : Level, date : Date.Date, message : String, details : List KeyValue }


log : Level -> Date.Date -> String -> List KeyValue -> Log
log lvl date message details =
    Log { lvl = lvl, date = date, message = message, details = details }


{-| Construct an information log.

It's the more common kind of log you can have. The short idea is, if it's not
a warning or an error then it's most probably an information ;).

-}
info : Date.Date -> String -> List KeyValue -> Log
info date message details =
    log Info date message details


{-| Construct a warning log.

Use a warning log when you want to attract the user attention about something
which is not critical. For example, if a HTTP request fail but we can retry it
just warn the user of the failure and that a retry is in progress.

-}
warn : Date.Date -> String -> List KeyValue -> Log
warn date message details =
    log Warn date message details


{-| Construct an error log.

An error log indicate a failure in the system. Most probably something happened
which have made the system unable to repair itself. For example, a HTTP request
which fail even after having retrying it.

-}
error : Date.Date -> String -> List KeyValue -> Log
error date message details =
    log Error date message details


{-| Build the view representing the given list of `Log`.

This is part of the Elm Architecture.

-}
view : List Log -> Html Messages.Msg
view logs =
    let
        entries =
            List.map logEntry logs
    in
    div [ id "log-container" ] entries


logEntry : Log -> Html Msg
logEntry log =
    case log of
        Log { lvl, date, message, details } ->
            let
                dateStr =
                    "[" ++ dateToString date ++ "]"

                messageClass =
                    case lvl of
                        Info ->
                            "info"

                        Warn ->
                            "warn"

                        Error ->
                            "error"

                detailsView d =
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
                ([ span [ class "date" ] [ text dateStr ]
                 , span [ class messageClass ] [ text message ]
                 ]
                    ++ (details |> List.sortBy sortByKeyValue |> List.map detailsView)
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

        400 ->
            "Bad Request"

        401 ->
            "Unauthorized"

        403 ->
            "Forbidden"

        _ ->
            "Unknown"
