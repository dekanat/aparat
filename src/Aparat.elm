module Aparat exposing (..)

import Common.Die exposing (Face, rollingDie)
import Common.Money exposing (Money)
import Random exposing (..)


type alias DiceRoll =
    ( Face, Face )


rollingPairOfDice : Random.Generator DiceRoll
rollingPairOfDice =
    Random.pair rollingDie rollingDie


calculatePayout : Money -> DiceRoll -> Money
calculatePayout betAmount ( rolledA, rolledB ) =
    if rolledA == rolledB then
        betAmount * 6

    else
        0
