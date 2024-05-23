module AccountingTests exposing (..)

import Accounting.Account as Account exposing (Account(..))
import Accounting.Accounting exposing (Request(..), updateWith)
import Common.Money exposing (Money)
import Expect exposing (..)
import Test exposing (..)


balanceOf : Account -> Money
balanceOf (Account balance) =
    balance


accountingTests : Test
accountingTests =
    describe "Accounting"
        [ describe "Withdraw"
            [ test "fulfills when the balance allows it" <|
                \() ->
                    Account 1200
                        |> updateWith
                            { fulfillOrder = always Expect.pass
                            , rejectOrder = Expect.fail "But balance was sufficient"
                            }
                            (Withdraw 1000)
                        |> Expect.all
                            [ Tuple.first >> balanceOf >> Expect.equal 200
                            , Tuple.second >> Maybe.withDefault (Expect.fail "Should yield")
                            ]
            , test "rejects when the balance does not allows it" <|
                \() ->
                    Account 800
                        |> updateWith
                            { fulfillOrder = always (Expect.fail "But balance was sufficient")
                            , rejectOrder = Expect.pass
                            }
                            (Withdraw 1000)
                        |> Expect.all
                            [ Tuple.first >> balanceOf >> Expect.equal 800
                            , Tuple.second >> Maybe.withDefault (Expect.fail "Should yield")
                            ]
            ]
        , test "Replenish" <|
            \() ->
                Account 800
                    |> updateWith
                        { fulfillOrder = always (Expect.fail "Nothing to yield")
                        , rejectOrder = Expect.fail "Nothing to yield"
                        }
                        (Replenish 200)
                    |> Expect.all
                        [ Tuple.first >> balanceOf >> Expect.equal 1000
                        , Tuple.second >> Maybe.withDefault Expect.pass
                        ]
        ]


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
        ]
