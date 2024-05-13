module AccountTests exposing (..)

import Account exposing (Account(..))
import Expect exposing (..)
import Test exposing (..)


playerAccountTests : Test
playerAccountTests =
    describe "Account"
        [ describe "Changing balance of an account"
            [ describe "add money"
                [ test "should increase balance correctly" <|
                    \() ->
                        Account 1000
                            |> Account.add 1000
                            |> Expect.equal (Account 2000)
                ]
            , describe "deduct"
                [ test "should reduce balance when sufficient" <|
                    \() ->
                        Account 1000
                            |> Expect.all
                                [ Account.deduct 100 >> Expect.equal (Ok (Account 900))
                                , Account.deduct 1000 >> Expect.equal (Ok (Account 0))
                                ]
                , test "should fail to reduce below zero" <|
                    \() ->
                        Account 1000
                            |> Account.deduct 2000
                            |> Expect.err
                ]
            ]
        , describe "Ask significant questions"
            [ test "should check if balance is at least asked amount" <|
                \() ->
                    Account 1000
                        |> Expect.all
                            [ Account.hasAtLeast 500 >> Expect.equal True
                            , Account.hasAtLeast 1000 >> Expect.equal True
                            , Account.hasAtLeast 1500 >> Expect.equal False
                            ]
            ]
        ]
