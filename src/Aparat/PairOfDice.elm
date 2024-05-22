module Aparat.PairOfDice exposing (..)

import Aparat.Core exposing (DieFace(..), PossibleCombination)
import Random


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
