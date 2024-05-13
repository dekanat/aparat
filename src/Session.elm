module Session exposing (..)

import Account exposing (Account)
import History exposing (History)
import Random


type alias Session outcomes =
    ( SessionState outcomes, Random.Seed )


type alias SessionState outcomes =
    { history : History outcomes
    , account : Account
    }


type SessionProblem
    = NonRecoverable
