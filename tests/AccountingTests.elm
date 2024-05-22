module AccountingTests exposing (..)

import Accounting exposing (Account(..), AccountingOps(..), AccountingResult(..), Exchange(..))
import Expect exposing (..)
import Test exposing (..)


accountingTests : Test
accountingTests =
    describe "Accounting"
        [ describe "Recieving a withdrawal request"
            [ test "when the balance allows it" <|
                \() ->
                    Accounting.update (WithdrawalRequest 2000) (Account 3000)
                        |> Expect.all
                            [ Expect.equal (Account 1000) << Tuple.first
                            , Expect.equal (ToOthers (WithdrawalSuccess 2000))
                                << Tuple.second
                            ]
            , test "when the balance does not allow it" <|
                \() ->
                    Accounting.update (WithdrawalRequest 2000) (Account 1000)
                        |> Expect.all
                            [ Expect.equal (Account 1000) << Tuple.first
                            , Expect.equal (ToOthers WithdrawalFailure)
                                << Tuple.second
                            ]
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
