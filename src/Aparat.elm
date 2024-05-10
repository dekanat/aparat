module Aparat exposing (..)

import Benzino exposing (Bet(..), RollOutcome)
import Common.Money exposing (Money)


type Payout
    = Win Money
    | Lose Money


determine : Bet -> RollOutcome -> Payout
determine bet rollOutcome =
    case ( bet, rollOutcome ) of
        ( Bet amount, ( rolledA, rolledB ) ) ->
            if rolledA == rolledB then
                Win (amount * 6)

            else
                Lose amount
