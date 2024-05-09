module MainTests exposing (..)

import Benzino exposing (Bet(..), RoundOutcome(..), RoundState(..))
import Balance exposing (Balance(..))
import Common.Die exposing (Face(..))
import Expect exposing (..)
import Main exposing (Msg(..), update)
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
                            Resolved ( Yek, Du ) (ReturnToPlayer 0)
                    in
                    { balance = Balance 3000, round = initialGameState }
                        |> update (PlayerWantsToBet 1000)
                        |> Tuple.first
                        |> Expect.equal { balance = Balance 2000, round = Initiated }
            , test "But balance is not sufficient" <|
                \() ->
                    let
                        initialGameState : RoundState
                        initialGameState =
                            Resolved ( Yek, Du ) (ReturnToPlayer 0)
                    in
                    { balance = Balance 500, round = initialGameState }
                        |> update (PlayerWantsToBet 1000)
                        |> Tuple.first
                        |> Expect.equal { balance = Balance 500, round = initialGameState }
            ]
        , describe "When the game resolves"
            [ test "And player wins" <|
                \() ->
                    let
                        winningCombination =
                            ( Yek, Yek )
                    in
                    { balance = Balance 2000, round = Initiated }
                        |> update (RoundResolves (Bet 1000) winningCombination)
                        |> Tuple.first
                        |> Expect.equal
                            { balance = Balance 8000
                            , round = Resolved winningCombination (ReturnToPlayer 6000)
                            }
            , test "And player loses" <|
                \() ->
                    let
                        losingRoll =
                            ( Yek, Du )
                    in
                    { balance = Balance 2000, round = Initiated }
                        |> update (RoundResolves (Bet 1000) losingRoll)
                        |> Tuple.first
                        |> Expect.equal
                            { balance = Balance 2000
                            , round = Resolved losingRoll (ReturnToPlayer 0)
                            }
            ]
        ]
