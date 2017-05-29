module Messages exposing (..)

import Date exposing (Date)
import Watch


type Msg
    = WatchMsg Watch.Msg
    | ResetAll
    | WithTime Date Msg
    | DisplayConfig
