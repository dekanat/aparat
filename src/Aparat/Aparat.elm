module Aparat.Aparat exposing (..)

import Aparat.Core exposing (PossibleCombination, winMultiplierFor)
import Aparat.Device exposing (fairPairOfDice)
import Common.Money exposing (Money)
import Random


type Request
    = InitiateRound Money


type alias Model =
    { seed : Random.Seed
    , lastEvent : Maybe PossibleCombination
    }


init : Random.Seed -> Model
init seed =
    Model seed Nothing


type alias Callback msg =
    { claimPayout : Money -> msg
    }


updateWith : Callback msg -> Request -> Model -> ( Model, Maybe msg )
updateWith { claimPayout } msg model =
    case msg of
        InitiateRound bet ->
            let
                ( settledCombination, nextSeed ) =
                    Random.step fairPairOfDice model.seed

                payout =
                    bet * winMultiplierFor settledCombination
            in
            ( Model nextSeed (Just settledCombination)
            , Just (claimPayout payout)
            )
