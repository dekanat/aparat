module Aparat.Core exposing (..)

import Common.Money exposing (Money)
import Random
import Task


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
    , lastEvent : Maybe PossibleCombination
    }


type alias RoundOutcome =
    { bet : Money
    , payout : Money
    }


type alias Args msg =
    { resolve : RoundOutcome -> Cmd msg }


type Msg
    = BetPlaced Money
