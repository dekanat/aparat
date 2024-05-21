module Session exposing (..)

import Accounting exposing (Account)
import Benzino


type Session e
    = NoSession
    | CurrentSession
        { account : Account
        , innerGame : Benzino.Model
        }


type SessionProblem
    = NonRecoverable
