module Aparat.Core exposing (..)


type alias PossibleCombination =
    ( DieFace, DieFace )


type DieFace
    = Yek
    | Du
    | Se
    | Jhar
    | Panj
    | Shesh


type alias Multiplier =
    Int


winMultiplierFor : PossibleCombination -> Multiplier
winMultiplierFor ( a, b ) =
    if a == b then
        6

    else
        0
