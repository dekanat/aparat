module Aparat.PayoutLogic exposing (..)

import Aparat.ChanceMechanism exposing (PossibleCombination)


type alias Multiplier =
    Int


winMultiplierFor : PossibleCombination -> Multiplier
winMultiplierFor ( a, b ) =
    if a == b then
        6

    else
        0
