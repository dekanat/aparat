module Benzino exposing (..)

import Bet exposing (Bet(..))
import Common exposing (Money)
import PairOfDice exposing (RollOutcome)


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
                        5

                    else
                        -1
            in
            ReturnToPlayer (amount * winScale)
