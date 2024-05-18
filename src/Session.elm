module Session exposing (..)

import Account exposing (Account)
import Common.Money exposing (Money)
import History exposing (History)
import List.Extra
import Random


type alias Session outcomes =
    ( SessionState outcomes, Random.Seed )


type Sess e
    = LoadingSession
    | CurrentSession
        { lastEvent : Maybe e
        , account : Account
        }
        Random.Seed


type alias SessionState outcomes =
    { history : History outcomes
    , account : Account
    }


type SessionProblem
    = NonRecoverable


balanceSettledThrough : SessionState e -> List Money
balanceSettledThrough { history, account } =
    let
        recoverEarlier event balanceAfterEvent =
            balanceAfterEvent - event.payout + event.bet
    in
    history |> List.Extra.scanr recoverEarlier (Account.balanceOf account)
