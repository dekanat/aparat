module Aparat.AparatTests exposing (..)

import Aparat.Aparat as Aparat exposing (Request(..))
import Aparat.Die exposing (Face(..))
import Aparat.PayoutLogic exposing (winMultiplierFor)
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
        initialState : Aparat.State
        initialState =
            { seed = Random.initialSeed 0
            , lastEvent = Nothing
            }

        update =
            Aparat.updateWith
                { claimPayout = always Expect.pass }

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
            initialState
                |> update (InitiateRound 100)
                |> Expect.all
                    [ Tuple.first >> .lastEvent >> ensureSomeValue
                    , Tuple.second >> Maybe.withDefault (Expect.fail "Should yield")
                    ]


aparatTests : Test
aparatTests =
    describe "Aparat"
        [ describe "Determine Payout"
            [ test "winning combination" <|
                \() ->
                    winMultiplierFor ( Panj, Panj )
                        |> Expect.equal 6
            , test "not winning combination" <|
                \() ->
                    winMultiplierFor ( Panj, Yek )
                        |> Expect.equal 0
            ]
        ]
