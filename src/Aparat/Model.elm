module Aparat.Model exposing (..)

import Aparat.Core exposing (PossibleCombination, winMultiplierFor)
import Aparat.Device exposing (fairPairOfDice)
import Common.Money exposing (Money)
import Random


type Msg
    = RoundInitiated Money


type alias Model =
    { seed : Random.Seed
    , lastEvent : Maybe PossibleCombination
    }


type alias Callback msg =
    { claimPayout : Money -> msg
    }


init : Random.Seed -> Model
init seed =
    Model seed Nothing


updateWith : Callback msg -> Msg -> Model -> ( Model, msg )
updateWith { claimPayout } msg model =
    case msg of
        RoundInitiated bet ->
            let
                ( settledCombination, nextSeed ) =
                    Random.step fairPairOfDice model.seed

                callback =
                    claimPayout (bet * winMultiplierFor settledCombination)
            in
            ( Model nextSeed (Just settledCombination)
            , callback
            )
