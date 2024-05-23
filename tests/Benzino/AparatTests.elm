module Benzino.AparatTests exposing (..)

import Aparat.Core exposing (DieFace(..), winMultiplierFor)
import Aparat.Shared
import Common.Money exposing (Money)
import Expect exposing (..)
import Fuzz exposing (..)
import Random
import Test exposing (..)


type OuterTypes
    = PayoutReceived Money


updateTests : Test
updateTests =
    let
        initialState : Aparat.Shared.Model
        initialState =
            { seed = Random.initialSeed 0
            , lastEvent = Nothing
            }

        update =
            Aparat.Shared.updateWith { claimPayout = PayoutReceived }

        ensureSomeValue : Maybe a -> Expectation
        ensureSomeValue m =
            case m of
                Just _ ->
                    Expect.pass

                _ ->
                    Expect.fail "No"
    in
    test "round resolves as expected" <|
        \() ->
            update (Aparat.Shared.BetPlaced 100) initialState
                |> Expect.all
                    [ Tuple.first >> .lastEvent >> ensureSomeValue
                    ]


aparatTests : Test
aparatTests =
    describe "Aparat"
        [ describe "Determine Payout"
            [ test "that apprat notifies of wins" <|
                \() ->
                    winMultiplierFor ( Panj, Panj )
                        |> Expect.equal 6
            , test "that apprat notifies of lose" <|
                \() ->
                    winMultiplierFor ( Panj, Yek )
                        |> Expect.equal 0
            ]
        ]
