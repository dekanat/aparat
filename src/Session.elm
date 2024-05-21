module Session exposing (..)

import Account exposing (Account)
import Benzino
import Random


type Session e
    = NoSession
    | CurrentSession
        { account : Account
        , innerGame : Benzino.Model
        }
        Random.Seed


type SessionProblem
    = NonRecoverable
