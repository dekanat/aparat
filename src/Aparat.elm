module Aparat exposing (..)

import Benzino exposing (RollOutcome)
import Common.Die exposing (Face, rollingDie)
import Common.Money exposing (Money)
import Random exposing (Seed)


type alias DiceRoll =
    ( Face, Face )


type alias Payout =
    Money


resolvePayout : Money -> DiceRoll -> Payout
resolvePayout amount settledOutcome =
    case settledOutcome of
        ( rolledA, rolledB ) ->
            if rolledA == rolledB then
                amount * 6

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


rollingPairOfDice : Random.Generator DiceRoll
rollingPairOfDice =
    Random.pair rollingDie rollingDie
