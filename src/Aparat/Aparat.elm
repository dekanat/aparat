module Aparat.Aparat exposing (..)

import Aggregate
import Aparat.Core exposing (PossibleCombination, winMultiplierFor)
import Aparat.Device exposing (fairPairOfDice)
import Common.Money exposing (Money)
import Random


type Request
    = InitiateRound Money


type alias State =
    { seed : Random.Seed
    , lastEvent : Maybe PossibleCombination
    }


init : Random.Seed -> State
init seed =
    State seed Nothing


type alias Callback msg =
    { claimPayout : Money -> msg
    }


updateWith : Callback msg -> Aggregate.Update State Request msg
updateWith { claimPayout } msg model =
    case msg of
        InitiateRound bet ->
            let
                ( settledCombination, nextSeed ) =
                    Random.step fairPairOfDice model.seed

                payout =
                    bet * winMultiplierFor settledCombination
            in
            ( State nextSeed (Just settledCombination)
            , Just (claimPayout payout)
            )
