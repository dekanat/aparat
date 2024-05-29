module Control.ControlTests exposing (..)

import Control.Account as Account exposing (Account, balanceOf)
import Control.Control as Control exposing (..)
import Expect
import Test exposing (Test, describe, test)


initControlTest : Test
initControlTest =
    describe "Initialize"
        [ describe "Bet size"
            [ test "start with minimum provided option" <|
                \() ->
                    Control.init 3000 [ 500, 1000, 5000 ]
                        |> .selectedBet
                        |> Expect.equal (Just 500)
            ]
        ]


bettingScenarios : Test
bettingScenarios =
    let
        initialBalance =
            3000

        possibleBet =
            500

        impossibleBet =
            5000

        initialState =
            Control.init initialBalance [ possibleBet, impossibleBet ]

        callbacks =
            { placeBet = \amount -> { betPlaced = amount }
            }

        update =
            Control.updateWith callbacks

        testBettingSuccessfully =
            let
                ( evolvedState, messaging ) =
                    initialState |> update (Bet possibleBet)
            in
            describe "Betting 500 when balance on account is 3000"
                [ test "should deduct betting amount from the account" <|
                    \() ->
                        evolvedState.account
                            |> Account.balanceOf
                            |> Expect.equal 2500
                , test "should notify on fulfilled order of 500" <|
                    \() ->
                        messaging
                            |> Expect.equal (Just (callbacks.placeBet possibleBet))
                ]

        testBettingWhenInsufficientBalance =
            let
                ( evolvedState, messaging ) =
                    initialState |> update (Bet impossibleBet)
            in
            describe "Betting 5000 when balance on account is 3000"
                [ test "should keep account balance as is" <|
                    \() ->
                        evolvedState.account
                            |> Account.balanceOf
                            |> Expect.equal initialBalance
                , test "should not resolve with callback when failed" <|
                    \() ->
                        messaging
                            |> Expect.equal Nothing
                ]
    in
    describe "Bet"
        [ testBettingSuccessfully
        , testBettingWhenInsufficientBalance
        ]


updateControlTest : Test
updateControlTest =
    describe "Update"
        [ bettingScenarios ]
