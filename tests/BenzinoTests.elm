module BenzinoTests exposing (..)

import Expect exposing (..)
import Main exposing (Accounts, DieFace(..), GameResult(..), RollResult, stake)
import Test exposing (..)
import Time exposing (Weekday(..))


juxtZarTests1 : Test
juxtZarTests1 =
    describe "Player Wins"
        [ test "When both dice are the same" <|
            \() ->
                stake 1000 ( Yek, Yek )
                    |> Expect.equal (MarkWins 6000)
        , test "Then Zara pays" <|
            \() ->
                let
                    accounts : Accounts
                    accounts =
                        { mark = 1000, zara = 50000 }

                    rollWinningCombination : () -> RollResult
                    rollWinningCombination () =
                        ( Yek, Yek )

                    settle : Accounts -> GameResult -> Accounts
                    settle initialAccounts result =
                        case result of
                            MarkWins amount ->
                                { mark = initialAccounts.mark + amount
                                , zara = initialAccounts.zara - amount
                                }

                            ZaraWins amount ->
                                { mark = initialAccounts.mark - amount
                                , zara = initialAccounts.zara + amount
                                }
                in
                rollWinningCombination ()
                    |> stake 1000
                    |> settle accounts
                    |> Expect.equal { mark = 7000, zara = 44000 }
        ]
