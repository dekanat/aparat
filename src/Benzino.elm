module Benzino exposing (..)

import Account exposing (Account(..))
import Aparat exposing (DiceRoll)
import Common.Money exposing (Money)
import History exposing (DeterminedEvent)
import Random
import Session exposing (Session, SessionProblem(..), SessionState)


type alias RoundDetails =
    DiceRoll


playOnce : Money -> Session DiceRoll -> Result SessionProblem (Session DiceRoll)
playOnce amountToBet ( aggregates, seed ) =
    let
        settleSessionState accountAfterBet =
            let
                determineOutcome : DiceRoll -> DeterminedEvent DiceRoll
                determineOutcome roll =
                    roll
                        |> Aparat.determinPayout amountToBet
                        |> DeterminedEvent seed roll amountToBet

                evolveState : DeterminedEvent DiceRoll -> SessionState DiceRoll
                evolveState event =
                    { history = aggregates.history |> History.add event
                    , account = accountAfterBet |> Account.add event.payout
                    }
            in
            seed
                |> Random.step Aparat.rollingPairOfDice
                |> Tuple.mapFirst (determineOutcome >> evolveState)
    in
    aggregates.account
        |> Account.deduct amountToBet
        |> Result.mapError (\_ -> NonRecoverable)
        |> Result.map settleSessionState
