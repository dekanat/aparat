module Session exposing (..)

import Account exposing (Account)
import Random


type Session e
    = NoSession
    | CurrentSession
        { lastEvent : Maybe e
        , account : Account
        }
        Random.Seed


type SessionProblem
    = NonRecoverable
