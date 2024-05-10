module Benzino exposing (..)

import Balance exposing (Balance(..), BalanceIssues(..))
import Common.Die exposing (Face, rollingDie)
import Common.Money exposing (Money)
import Random


type Bet
    = Bet Money


type RoundOutcome
    = ReturnToPlayer Money


type alias RollOutcome =
    ( Face, Face )


type RoundState
    = Initiated
    | Resolved RollOutcome RoundOutcome


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


determinePayout : RollOutcome -> Bet -> RoundOutcome
determinePayout ( rolledA, rolledB ) bet =
    case bet of
        Bet amount ->
            let
                winScale =
                    if rolledA == rolledB then
                        6

                    else
                        0
            in
            ReturnToPlayer (amount * winScale)
