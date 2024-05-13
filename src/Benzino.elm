module Benzino exposing (..)

import Account exposing (Account(..))
import Aparat exposing (DiceRoll)
import Common.Money exposing (Money)
import Core exposing (SessionContext, SessionProblem(..))
import History exposing (DeterminedEvent, History)
import Random


type alias Bet =
    Money


type alias BenzinoEvent =
    DeterminedEvent DiceRoll


type alias BenzinoAggregates =
    { history : History DiceRoll
    , account : Account
    }


type alias BenzinoContext =
    SessionContext BenzinoAggregates


playOnce : Money -> SessionContext BenzinoAggregates -> Result SessionProblem (SessionContext BenzinoAggregates)
playOnce amountToBet ( aggregates, seed ) =
    let
        resolveBet : Bet -> Account -> BenzinoContext
        resolveBet betAmount accountAfterBet =
            let
                settleAggregates : BenzinoEvent -> BenzinoAggregates
                settleAggregates event =
                    { history = aggregates.history |> History.add event
                    , account = accountAfterBet |> Account.add event.payout
                    }
            in
            seed
                |> generateOutcome betAmount
                |> Tuple.mapFirst settleAggregates
    in
    aggregates.account
        |> Account.deduct amountToBet
        |> Result.mapError (\_ -> NonRecoverable)
        |> Result.map (resolveBet amountToBet)


type alias RollingRandom =
    ( BenzinoEvent, Random.Seed )


generateOutcome : Money -> Random.Seed -> RollingRandom
generateOutcome bet seed =
    let
        resolveEvent : DiceRoll -> BenzinoEvent
        resolveEvent roll =
            roll
                |> Aparat.determinPayout bet
                |> DeterminedEvent roll seed bet
    in
    seed
        |> Random.step Aparat.rollingPairOfDice
        |> Tuple.mapFirst resolveEvent
