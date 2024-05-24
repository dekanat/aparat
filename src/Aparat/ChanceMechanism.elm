module Aparat.ChanceMechanism exposing (..)

import Aparat.Die as Die exposing (Face(..))
import Random


type alias PossibleCombination =
    ( Die.Face, Die.Face )


fairPairOfDice : Random.Generator PossibleCombination
fairPairOfDice =
    let
        fairDie : Random.Generator Die.Face
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
