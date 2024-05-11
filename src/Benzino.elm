module Benzino exposing (..)

import Account exposing (Account(..))
import Aparat exposing (DiceRoll)
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
                        |> generateOutcome betAmount
                        |> settleWithOutcome
            in
            aggregates.account
                |> Account.deduct amountToBet
                |> Result.mapError (\_ -> NonRecoverable)
                |> Result.map (resolveBet amountToBet)


type alias DeterminedEvent =
    { seed : Random.Seed
    , bet : Money
    , roll : DiceRoll
    , payout : Money
    }


type alias RandomOutcome =
    ( DeterminedEvent, Random.Seed )


generateOutcome : Money -> Random.Seed -> ( DeterminedEvent, Random.Seed )
generateOutcome bet seed =
    let
        resolveEvent : DiceRoll -> DeterminedEvent
        resolveEvent roll =
            roll
                |> Aparat.determinPayout bet
                |> DeterminedEvent seed bet roll
    in
    seed
        |> Random.step Aparat.rollingPairOfDice
        |> Tuple.mapFirst resolveEvent
