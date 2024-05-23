module Benzino.AparatTests exposing (..)

import Aparat.Benzino
import Aparat.Core as Core exposing (..)
import Aparat.PairOfDice exposing (fairPairOfDice)
import Common.Money exposing (Money)
import Expect exposing (..)
import Fuzz exposing (..)
import Random
import Task
import Test exposing (..)


type OuterTypes
    = RoundResolved Money


type alias Multiplier =
    Int


payoutMultiplierFor : PossibleCombination -> Multiplier
payoutMultiplierFor ( a, b ) =
    if a == b then
        6

    else
        0


type alias Callbacks msg =
    { settlePayout : Money -> msg
    }


type Outcome
    = Payout Money


update : Msg -> Model -> ( Model, Outcome )
update msg model =
    case msg of
        BetPlaced bet ->
            let
                ( settledCombination, nextSeed ) =
                    Random.step fairPairOfDice model.seed

                callback =
                    Payout (bet * payoutMultiplierFor settledCombination)

                nextModel =
                    { model
                        | seed = nextSeed
                        , lastEvent = Just settledCombination
                    }
            in
            ( nextModel
            , callback
            )


updateTests : Test
updateTests =
    let
        initialState : Core.Model
        initialState =
            { seed = Random.initialSeed 0
            , lastEvent = Nothing
            }

        ensurePresent : Maybe a -> Expectation
        ensurePresent =
            Maybe.map (always True)
                >> Maybe.withDefault False
                >> Expect.equal True

        -- ( evolvedState, producedCmd ) =
    in
    test "round resolves as expected" <|
        \() ->
            update (Core.BetPlaced 100) initialState
                |> Expect.all
                    [ Tuple.first >> .lastEvent >> ensurePresent
                    ]


aparatTests : Test
aparatTests =
    describe "Aparat"
        [ describe "Determine Payout"
            [ test "that apprat notifies of wins" <|
                \() ->
                    Aparat.Benzino.calculatePayout 100 ( Panj, Panj )
                        |> Expect.equal 600
            , test "that apprat notifies of lose" <|
                \() ->
                    Aparat.Benzino.calculatePayout 100 ( Panj, Yek )
                        |> Expect.equal 0
            ]
        ]
