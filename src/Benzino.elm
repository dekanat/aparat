module Benzino exposing (..)

import Account exposing (Account(..))
import Aparat exposing (DeterminedEvent, RandomOutcome)
import Common.Money exposing (Money)
import Random


type alias Bet =
    Money


type alias SessionAggregates =
    { history : List DeterminedEvent
    , account : Account
    }


type SessionContext
    = SettledSession SessionAggregates Random.Seed


type SessionProblem
    = NonRecoverable


playOnce : Money -> SessionContext -> Result SessionProblem SessionContext
playOnce amountToBet session =
    case session of
        SettledSession aggregates seed ->
            let
                resolveBet : Bet -> Account -> SessionContext
                resolveBet betAmount accountAfterBet =
                    let
                        settleWithOutcome : RandomOutcome -> SessionContext
                        settleWithOutcome ( event, nextSeed ) =
                            SettledSession
                                (SessionAggregates
                                    (event :: aggregates.history)
                                    (accountAfterBet |> Account.add event.payout)
                                )
                                nextSeed
                    in
                    seed
                        |> Aparat.resolveOutcome betAmount
                        |> settleWithOutcome
            in
            aggregates.account
                |> Account.deduct amountToBet
                |> Result.mapError (\_ -> NonRecoverable)
                |> Result.map (resolveBet amountToBet)
