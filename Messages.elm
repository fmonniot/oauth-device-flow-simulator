module Messages exposing (..)

{-| This module expose root messages of the application.

It has its own module as its required by both `Main` and `Log`, and
would otherwise create cyclic dependencies.


# Elm Architecture

@docs Msg

-}

import Date exposing (Date)
import Watch


{-|

  - `NoOp` will do nothing
  - `WatchMsg` is a 'container' type for the messages defined in the `Watch` module
  - `ResetAll` will reset the application to its original state
  - `WithTime` is a container type for `Msg` itself. It let us have access to the `Date` at
    which the message have been emitted. Most useful for the logs.
  - `DisplayConfig` will print the current application configuration in the logs.

-}
type Msg
    = NoOp
    | WatchMsg Watch.Msg
    | ResetAll
    | WithTime Date Msg
    | DisplayConfig
