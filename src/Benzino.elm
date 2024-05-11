module Benzino exposing (..)

import Balance exposing (Balance(..), BalanceIssues(..))
import Common.Die exposing (Face, rollingDie)
import Common.Money exposing (Money)
import Random


type Bet
    = Bet Money


type RoundOutcome
    = Money


type alias RollOutcome =
    ( Face, Face )


type RoundState
    = Initiated
    | Resolved RollOutcome Money


makeBet : Money -> Balance -> Result BalanceIssues ( Bet, Balance )
makeBet amountToBet balance =
    let
        acceptBet reducedBalance =
            ( Bet amountToBet, reducedBalance )
    in
    Balance.takeFrom balance amountToBet
        |> Result.map acceptBet


rollingPairOfDice : Random.Generator RollOutcome
rollingPairOfDice =
    Random.pair rollingDie rollingDie
