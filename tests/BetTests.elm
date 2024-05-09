module BetTests exposing (..)

import Bet exposing (..)
import Expect exposing (..)
import Result exposing (..)
import Test exposing (..)


updateTests : Test
updateTests =
    describe "makeBet"
        [ test "successfully makes a bet" <|
            \() ->
                makeBet 1000 100
                    |> Expect.equal (Ok ( 900, Bet 100 ))
        , test "fails to make a over-the-budget bets" <|
            \() ->
                makeBet 1300 1500
                    |> Expect.equal (Err NotEnoughAmount)
        ]
