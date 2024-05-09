module BenzinoTests exposing (..)

import Bet exposing (Bet(..))
import Die exposing (Face(..))
import Expect exposing (..)
import Main exposing (GameResult(..), Msg(..), RoundState(..), evaluateGameResult, update)
import Test exposing (..)
import Time exposing (Weekday(..))


updateTests : Test
updateTests =
    describe "update"
        [ describe "When player makes a bet"
            [ test "And it's accepted" <|
                \() ->
                    let
                        initialGameState : RoundState
                        initialGameState =
                            Resolved ( Yek, Du ) (MarkWins 0)
                    in
                    { balance = 3000, round = initialGameState }
                        |> update (PlayerBets 1000)
                        |> Tuple.first
                        |> Expect.equal { balance = 2000, round = Initiated }
            , test "But balance is not sufficient" <|
                \() ->
                    let
                        initialGameState : RoundState
                        initialGameState =
                            Resolved ( Yek, Du ) (MarkWins 0)
                    in
                    { balance = 500, round = initialGameState }
                        |> update (PlayerBets 1000)
                        |> Tuple.first
                        |> Expect.equal { balance = 500, round = initialGameState }
            ]
        , describe "When the game resolves"
            [ test "And player wins" <|
                \() ->
                    let
                        winningCombination =
                            ( Yek, Yek )
                    in
                    { balance = 2000, round = Initiated }
                        |> update (RoundResolves (Bet 1000) winningCombination)
                        |> Tuple.first
                        |> Expect.equal
                            { balance = 8000
                            , round = Resolved winningCombination (MarkWins 6000)
                            }
            , test "And player loses" <|
                \() ->
                    let
                        losingRoll =
                            ( Yek, Du )
                    in
                    { balance = 2000, round = Initiated }
                        |> update (RoundResolves (Bet 1000) losingRoll)
                        |> Tuple.first
                        |> Expect.equal
                            { balance = 2000
                            , round = Resolved losingRoll (MarkWins 0)
                            }
            ]
        ]


gameRulesTests : Test
gameRulesTests =
    describe "Player Wins"
        [ test "When both dice are the same" <|
            \() ->
                Bet 1000
                    |> evaluateGameResult ( Yek, Yek )
                    |> Expect.equal (MarkWins 6000)
        ]
