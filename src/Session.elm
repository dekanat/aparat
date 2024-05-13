module Session exposing (..)

import Account exposing (Account)
import History exposing (History, RoundSnapshot)
import Random


type alias Session outcomes =
    ( SessionState outcomes, Random.Seed )


type alias SessionState outcomes =
    { history : History outcomes
    , account : Account
    }


type SessionProblem
    = NonRecoverable


evolveState : SessionState e -> RoundSnapshot e -> SessionState e
evolveState currentState snapshot =
    { history = currentState.history |> History.record snapshot
    , account = currentState.account |> Account.add (snapshot |> Tuple.second |> .payout)
    }
