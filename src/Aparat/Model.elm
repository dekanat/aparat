module Aparat.Model exposing (..)

import Common.Money exposing (Money)
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


type alias Model =
    { seed : Random.Seed
    , event : PossibleCombination
    , bet : Money
    }


type alias NextModel =
    { presentCombination : Maybe PossibleCombination
    }
