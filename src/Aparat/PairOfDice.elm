module Aparat.PairOfDice exposing (..)

import Random


type alias PossibleCombination =
    ( DieFace, DieFace )


type DieFace
    = Yek
    | Du
    | Se
    | Jhar
    | Panj
    | Shesh


fairPairOfDice : Random.Generator PossibleCombination
fairPairOfDice =
    let
        fairDie : Random.Generator DieFace
        fairDie =
            Random.uniform Yek
                [ Du
                , Se
                , Jhar
                , Panj
                , Shesh
                ]
    in
    Random.pair fairDie fairDie
