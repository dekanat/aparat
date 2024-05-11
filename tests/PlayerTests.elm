module PlayerTests exposing (..)

import Aparat exposing (Payout)
import Balance exposing (Balance(..))
import Expect exposing (..)
import Player exposing (..)
import Test exposing (..)


playerTests : Test
playerTests =
    describe "Player"
        [ describe "makeBet"
            [ test "successfully makes a bet" <|
                \() ->
                    Balance 1000
                        |> makeBet 100
                        |> Expect.equal
                            (Ok
                                ( 100
                                , Balance 900
                                )
                            )
            , test "fails to make a over-the-budget bets" <|
                \() ->
                    Balance 1300
                        |> makeBet 1500
                        |> Expect.err
            ]
        , describe "collect"
            [ test "that the player can consolidate" <|
                \() ->
                    Balance 100
                        |> Expect.all
                            [ roundUp 200 >> Expect.equal (Balance 300)
                            , roundUp 0 >> Expect.equal (Balance 100)
                            ]
            ]
        ]
