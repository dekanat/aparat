module Accounting.AccountingTests exposing (..)

import Accounting.Accounting exposing (Request(..), updateWith)
import Common.Money exposing (Money)
import ControlPanel.Account exposing (Account(..))
import Expect
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
