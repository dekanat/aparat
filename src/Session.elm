module Session exposing (..)

import Accounting exposing (Account)
import Aparat.Benzino


type Session e
    = NoSession
    | CurrentSession
        { account : Account
        , innerGame : Aparat.Benzino.Model
        }


type SessionProblem
    = NonRecoverable
