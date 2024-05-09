module Benzino exposing (..)

import Balance exposing (Balance(..), BalanceIssues(..))
import Common.Die exposing (Face, rollingDie)
import Common.Money exposing (Money)
import Random


type RoundState
    = Initiated
    | Resolved RollOutcome RoundOutcome


type RoundOutcome
    = ReturnToPlayer Money


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


type alias RollOutcome =
    ( Face, Face )


rollingPairOfDice : Random.Generator RollOutcome
rollingPairOfDice =
    Random.pair rollingDie rollingDie


type Bet
    = Bet Money


makeBet : Money -> Balance -> Result BalanceIssues ( Bet, Balance )
makeBet amountToBet balance =
    case Balance.takeFrom balance amountToBet of
        Ok newBalance ->
            Ok ( Bet amountToBet, newBalance )

        Err any ->
            Err any
