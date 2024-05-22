module Aparat.Model exposing (..)

import Aparat.Core exposing (PossibleCombination)
import Common.Money exposing (Money)
import Random


type alias Model =
    { seed : Random.Seed
    , event : PossibleCombination
    , bet : Money
    }


type alias NextModel =
    { presentCombination : Maybe PossibleCombination
    }
