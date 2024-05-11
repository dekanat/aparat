module BalanceTests exposing (..)

import Balance exposing (Balance(..), BalanceIssues(..))
import Benzino exposing (Bet(..), RoundOutcome(..), RoundState(..))
import Common.Die exposing (Face(..))
import Expect exposing (..)
import Test exposing (..)
import Time exposing (Weekday(..))


balanceTests : Test
balanceTests =
    describe "operations"
        [ describe "topUp"
            [ test "should increas balance correctly" <|
                \() ->
                    Balance.topUp (Balance 1000) 1000
                        |> Expect.equal (Balance 2000)
            ]
        , describe "takeFrom"
            [ test "should reduce balance when sufficient" <|
                \() ->
                    ( Balance.takeFrom (Balance 1000) 1000
                    , Balance.takeFrom (Balance 3000) 1000
                    )
                        |> Expect.equal
                            ( Ok (Balance 0)
                            , Ok (Balance 2000)
                            )
            , test "should fail to reduce below zero" <|
                \() ->
                    Balance.takeFrom (Balance 500) 1000
                        |> Expect.equal (Err InsufficientBalance)
            ]
        ]
