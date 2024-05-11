module MainTests exposing (..)

import Account exposing (Account(..))
import Benzino exposing (Bet(..), RoundOutcome(..), RoundState(..))
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
                            Resolved ( Yek, Du ) 0
                    in
                    { account = Account 3000
                    , round = initialGameState
                    }
                        |> update (PlayerWantsToBet 1000)
                        |> Tuple.first
                        |> Expect.equal { account = Account 2000, round = Initiated }
            , test "But balance is not sufficient" <|
                \() ->
                    let
                        initialGameState : RoundState
                        initialGameState =
                            Resolved ( Yek, Du ) 0
                    in
                    { account = Account 500, round = initialGameState }
                        |> update (PlayerWantsToBet 1000)
                        |> Tuple.first
                        |> Expect.equal { account = Account 500, round = initialGameState }
            ]
        , describe "When the game resolves"
            [ test "And player wins" <|
                \() ->
                    let
                        winningCombination =
                            ( Yek, Yek )
                    in
                    { account = Account 2000
                    , round = Initiated
                    }
                        |> update (RoundResolves 1000 winningCombination)
                        |> Tuple.first
                        |> Expect.equal
                            { account = Account 8000
                            , round = Resolved winningCombination 6000
                            }
            , test "And player loses" <|
                \() ->
                    let
                        losingRoll =
                            ( Yek, Du )
                    in
                    { account = Account 2000, round = Initiated }
                        |> update (RoundResolves 1000 losingRoll)
                        |> Tuple.first
                        |> Expect.equal
                            { account = Account 2000
                            , round = Resolved losingRoll 0
                            }
            ]
        ]
