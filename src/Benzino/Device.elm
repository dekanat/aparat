module Benzino.Device exposing (..)

import Random


type Face
    = Yek
    | Du
    | Se
    | Jhar
    | Panj
    | Shesh


rollingDie : Random.Generator Face
rollingDie =
    Random.uniform Yek
        [ Du
        , Se
        , Jhar
        , Panj
        , Shesh
        ]


type alias DiceRoll =
    ( Face, Face )


fairDiceRoll : Random.Generator DiceRoll
fairDiceRoll =
    Random.pair rollingDie rollingDie
