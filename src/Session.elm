module Session exposing (..)

import Account exposing (Account)
import History exposing (History)
import Random


type alias Session e =
    ( SessionState e, Random.Seed )


type alias SessionState e =
    { history : History e
    , account : Account
    }


type SessionProblem
    = NonRecoverable
