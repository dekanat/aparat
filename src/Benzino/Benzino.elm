module Benzino.Benzino exposing (..)

import Benzino.Device exposing (DiceRoll, fairDiceRoll)
import Common.Money exposing (Money)
import Random exposing (..)
import Round exposing (..)


calculatePayout : Money -> DiceRoll -> Money
calculatePayout betAmount ( rolledA, rolledB ) =
    if rolledA == rolledB then
        betAmount * 6

    else
        0


benzino =
    GameOfChance fairDiceRoll calculatePayout
