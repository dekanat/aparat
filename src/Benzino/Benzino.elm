module Benzino.Benzino exposing (..)

import Benzino.Die exposing (Face, rollingDie)
import Common.Money exposing (Money)
import Random exposing (..)
import Round exposing (..)


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


benzino =
    GameOfChance rollingPairOfDice calculatePayout
