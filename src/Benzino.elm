module Benzino exposing (..)

import Common.Balance exposing (Balance(..))
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


type BettingDifficulties
    = NotEnoughAmount


makeBet : Balance -> Money -> Result BettingDifficulties ( Bet, Balance )
makeBet balance amountToBet =
    case balance of
        Balance availableFunds ->
            if amountToBet < availableFunds then
                Ok
                    ( Bet amountToBet
                    , Balance (availableFunds - amountToBet)
                    )

            else
                Err NotEnoughAmount
