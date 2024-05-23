module Aparat.Shared exposing (..)

import Aparat.Core exposing (PossibleCombination, winMultiplierFor)
import Aparat.PairOfDice exposing (fairPairOfDice)
import Common.Money exposing (Money)
import Random


type Msg
    = BetPlaced Money


type alias Model =
    { seed : Random.Seed
    , lastEvent : Maybe PossibleCombination
    }


type alias Callback msg =
    { claimPayout : Money -> msg
    }


updateWith : Callback msg -> Msg -> Model -> ( Model, msg )
updateWith { claimPayout } msg model =
    case msg of
        BetPlaced bet ->
            let
                ( settledCombination, nextSeed ) =
                    Random.step fairPairOfDice model.seed

                callback =
                    claimPayout (bet * winMultiplierFor settledCombination)
            in
            ( Model nextSeed (Just settledCombination)
            , callback
            )
