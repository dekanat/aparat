module Benzino exposing (..)

import Account exposing (Account(..))
import Aparat exposing (DiceRoll)
import Common.Money exposing (Money)
import History
import Random
import Round exposing (Round)
import Session exposing (Session, SessionProblem(..), SessionState)


type alias RoundDetails =
    DiceRoll


playOnce : Money -> Session DiceRoll -> Result SessionProblem (Session DiceRoll)
playOnce amountToBet ( aggregates, seed ) =
    let
        settleSessionState accountAfterBet =
            let
                settleRound : DiceRoll -> Round DiceRoll
                settleRound roll =
                    roll
                        |> Aparat.calculatePayout amountToBet
                        |> Round seed roll amountToBet

                evolveState : Round DiceRoll -> SessionState DiceRoll
                evolveState event =
                    { history = aggregates.history |> History.add event
                    , account = accountAfterBet |> Account.add event.payout
                    }
            in
            seed
                |> Random.step Aparat.rollingPairOfDice
                |> Tuple.mapFirst (settleRound >> evolveState)
    in
    aggregates.account
        |> Account.deduct amountToBet
        |> Result.mapError (\_ -> NonRecoverable)
        |> Result.map settleSessionState
