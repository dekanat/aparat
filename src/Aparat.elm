module Aparat exposing (..)

import Benzino exposing (RollOutcome)
import Common.Die exposing (Face, rollingDie)
import Common.Money exposing (Money)
import Random exposing (Seed)


type Bet
    = Bet Money


type alias RollOutcome =
    ( Face, Face )


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


type alias RoundResolution =
    { payout : Payout
    , rollOutcome : RollOutcome
    }


playRound : Seed -> Bet -> ( RoundResolution, Seed )
playRound seed bet =
    let
        ( rollOutcome, newSeed ) =
            Random.step rollingPairOfDice seed

        payout =
            determine bet rollOutcome
    in
    ( { payout = payout, rollOutcome = rollOutcome }, newSeed )


rollingPairOfDice : Random.Generator RollOutcome
rollingPairOfDice =
    Random.pair rollingDie rollingDie
