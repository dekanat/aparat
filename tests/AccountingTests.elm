module AccountingTests exposing (..)

import Accounting exposing (Account(..), Request(..))
import Expect exposing (..)
import Test exposing (..)


accountingTests : Test
accountingTests =
    describe "Accounting"
        [ describe "Recieving a withdrawal request"
            [ todo "when the balance allows it"
            , todo "when the balance does not allow it"
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
                            |> Accounting.add 1000
                            |> Expect.equal (Account 2000)
                ]
            , describe "deduct"
                [ test "should reduce balance when sufficient" <|
                    \() ->
                        Account 1000
                            |> Expect.all
                                [ Accounting.deduct 100 >> Expect.equal (Ok (Account 900))
                                , Accounting.deduct 1000 >> Expect.equal (Ok (Account 0))
                                ]
                , test "should fail to reduce below zero" <|
                    \() ->
                        Account 1000
                            |> Accounting.deduct 2000
                            |> Expect.err
                ]
            ]
        , describe "Ask significant questions"
            [ test "should check if balance is at least asked amount" <|
                \() ->
                    Account 1000
                        |> Expect.all
                            [ Accounting.hasAtLeast 500 >> Expect.equal True
                            , Accounting.hasAtLeast 1000 >> Expect.equal True
                            , Accounting.hasAtLeast 1500 >> Expect.equal False
                            ]
            ]
        ]
