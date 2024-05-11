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


rollingPairOfDice : Random.Generator DiceRoll
rollingPairOfDice =
    Random.pair rollingDie rollingDie
