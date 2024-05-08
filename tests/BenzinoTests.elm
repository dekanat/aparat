module BenzinoTests exposing (..)

import Expect exposing (..)
import Main exposing (DieFace(..), GameResult(..), GameState(..), Msg(..), evaluateGameResult, update)
import Test exposing (..)
import Time exposing (Weekday(..))


updateTests : Test
updateTests =
    describe "update"
        [ test "When a bet is placed" <|
            \() ->
                let
                    initialGameState : GameState
                    initialGameState =
                        Resolved ( Yek, Du ) (MarkWins 0)
                in
                { balance = 3000, gameState = initialGameState }
                    |> update (Bet 1000)
                    |> Tuple.first
                    |> Expect.equal { balance = 2000, gameState = Staked 1000 }
        , describe "When the game resolves"
            [ test "And player wins" <|
                \() ->
                    let
                        winningCombination =
                            ( Yek, Yek )
                    in
                    { balance = 2000, gameState = Staked 1000 }
                        |> update (GameResolves winningCombination)
                        |> Tuple.first
                        |> Expect.equal
                            { balance = 8000
                            , gameState = Resolved winningCombination (MarkWins 6000)
                            }
            , test "And player loses" <|
                \() ->
                    let
                        losingRoll =
                            ( Yek, Du )
                    in
                    { balance = 2000, gameState = Staked 1000 }
                        |> update (GameResolves losingRoll)
                        |> Tuple.first
                        |> Expect.equal
                            { balance = 2000
                            , gameState = Resolved losingRoll (ZaraWins 1000)
                            }
            ]
        ]


gameRulesTests : Test
gameRulesTests =
    describe "Player Wins"
        [ test "When both dice are the same" <|
            \() ->
                evaluateGameResult 1000 ( Yek, Yek )
                    |> Expect.equal (MarkWins 6000)
        ]
