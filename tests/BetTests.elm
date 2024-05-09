module BetTests exposing (..)

import Balance exposing (Balance(..))
import Bet exposing (..)
import Expect exposing (..)
import Result exposing (..)
import Test exposing (..)


updateTests : Test
updateTests =
    describe "makeBet"
        [ test "successfully makes a bet" <|
            \() ->
                makeBet (Balance 1000) 100
                    |> Expect.equal (Ok ( Bet 100, Balance 900 ))
        , test "fails to make a over-the-budget bets" <|
            \() ->
                makeBet (Balance 1300) 1500
                    |> Expect.equal (Err NotEnoughAmount)
        ]
