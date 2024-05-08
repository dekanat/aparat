module BenzinoTests exposing (..)

import Expect exposing (..)
import Main exposing (Accounts, DieFace(..), GameResult(..), GameState(..), Model, Msg(..), RollResult, stake, update)
import Random
import Test exposing (..)
import Time exposing (Weekday(..))


updateTests : Test
updateTests =
    describe "update"
        [ test "When a bet is placed" <|
            \() ->
                let
                    defaultGameState : GameState
                    defaultGameState =
                        Resolved ( Yek, Du ) (MarkWins 0)

                    initialModel : Model
                    initialModel =
                        { balance = 3000, gameState = defaultGameState }
                in
                update (Bet 1000) initialModel
                    |> Tuple.first
                    |> Expect.equal { balance = 2000, gameState = Staked 1000 }
        , test "When the game resolves" <|
            \() ->
                let
                    initialModel : Model
                    initialModel =
                        { balance = 2000, gameState = Staked 1000 }

                    newGameState : GameState
                    newGameState =
                        Resolved ( Yek, Yek ) (MarkWins 6000)
                in
                update (GameResolves ( Yek, Yek )) initialModel
                    |> Tuple.first
                    |> Expect.equal { balance = 8000, gameState = newGameState }
        ]



--    PlayerBets _ ->
--         ( model
--         , Random.generate GameResolves dieRoller
--         )
--     GameResolves rollResults ->
--         ( rollResults
--         , Cmd.none
--         )
--     DetermineWin _ ->
--         ( model
--         , Cmd.none
--         )


dieRoller : Random.Generator RollResult
dieRoller =
    Random.pair dieGenerator dieGenerator


dieGenerator : Random.Generator DieFace
dieGenerator =
    Random.uniform Yek
        [ Du
        , Se
        , Jhar
        , Panj
        , Shesh
        ]


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
