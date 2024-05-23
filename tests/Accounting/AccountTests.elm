module Accounting.AccountTests exposing (..)

import Accounting.Account as Account exposing (Account(..))
import Accounting.Accounting exposing (Request(..))
import Expect exposing (..)
import Test exposing (..)


accountTests : Test
accountTests =
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
        ]
