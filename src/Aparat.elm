module Aparat exposing (..)

import Benzino exposing (RollOutcome)
import Common.Die exposing (Face, rollingDie)
import Common.Money exposing (Money)
import Random exposing (Seed)


type alias DiceRoll =
    ( Face, Face )


resolvePayout : Money -> DiceRoll -> Money
resolvePayout betAmount ( rolledA, rolledB ) =
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


playRound : Seed -> Money -> ( DeterminedEvent, Seed )
playRound seed bet =
    let
        resolveEvent : RollOutcome -> DeterminedEvent
        resolveEvent roll =
            roll
                |> resolvePayout bet
                |> DeterminedEvent seed bet roll
    in
    seed
        |> Random.step rollingPairOfDice
        |> Tuple.mapFirst resolveEvent


type alias RandomOutcome =
    ( DeterminedEvent, Seed )


determineOutcome : Money -> Seed -> ( DeterminedEvent, Seed )
determineOutcome bet seed =
    playRound seed bet


rollingPairOfDice : Random.Generator DiceRoll
rollingPairOfDice =
    Random.pair rollingDie rollingDie
