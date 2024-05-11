module Aparat exposing (..)

import Common.Die exposing (Face, rollingDie)
import Common.Money exposing (Money)
import Random exposing (Seed)


type alias DiceRoll =
    ( Face, Face )


determinPayout : Money -> DiceRoll -> Money
determinPayout betAmount ( rolledA, rolledB ) =
    if rolledA == rolledB then
        betAmount * 6

    else
        0


type alias DeterminedEvent =
    { seed : Seed
    , bet : Money
    , roll : DiceRoll
    , payout : Money
    }


type alias RandomOutcome =
    ( DeterminedEvent, Seed )


resolveOutcome : Money -> Seed -> ( DeterminedEvent, Seed )
resolveOutcome bet seed =
    let
        resolveEvent : DiceRoll -> DeterminedEvent
        resolveEvent roll =
            roll
                |> determinPayout bet
                |> DeterminedEvent seed bet roll
    in
    seed
        |> Random.step rollingPairOfDice
        |> Tuple.mapFirst resolveEvent


rollingPairOfDice : Random.Generator DiceRoll
rollingPairOfDice =
    Random.pair rollingDie rollingDie
