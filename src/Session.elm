module Session exposing (..)

import Accounting exposing (Account)
import Aparat.Model


type Session e
    = NoSession
    | CurrentSession
        { account : Account
        , innerGame : Aparat.Model.Model
        }


type SessionProblem
    = NonRecoverable
