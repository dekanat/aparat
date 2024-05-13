module Benzino exposing (..)

import Account exposing (Account(..))
import Aparat exposing (DiceRoll)
import Common.Money exposing (Money)
import History exposing (RoundSnapshot)
import Random
import Round exposing (Round)
import Session exposing (Session, SessionProblem(..), SessionState)


type alias RoundDetails =
    DiceRoll


playOnce : Money -> Session DiceRoll -> Result SessionProblem (Session DiceRoll)
playOnce amountToBet ( aggregates, seed ) =
    let
        evolveSessionState : ( RoundSnapshot DiceRoll, Random.Seed ) -> ( SessionState DiceRoll, Random.Seed )
        evolveSessionState =
            Tuple.mapFirst (Session.evolveState aggregates)

        settleRoundSnapshot accountDuringRound =
            let
                settleRound : DiceRoll -> Round DiceRoll
                settleRound roll =
                    roll
                        |> Aparat.calculatePayout amountToBet
                        |> Round seed roll amountToBet

                completeSnapshot =
                    Tuple.pair accountDuringRound
            in
            seed
                |> Random.step Aparat.rollingPairOfDice
                |> Tuple.mapFirst (settleRound >> completeSnapshot)
    in
    aggregates.account
        |> Account.deduct amountToBet
        |> Result.mapError (\_ -> NonRecoverable)
        |> Result.map (settleRoundSnapshot >> evolveSessionState)
