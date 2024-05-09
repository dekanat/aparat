module BenzinoTests exposing (..)

import Benzino exposing (Balance(..), Bet(..), BettingDifficulties(..), RoundOutcome(..), makeBet)
import Die exposing (Face(..))
import Expect exposing (..)
import Result exposing (..)
import Test exposing (..)


betTests : Test
betTests =
    describe "makeBet"
        [ test "successfully makes a bet" <|
            \() ->
                makeBet (Balance 1000) 100
                    |> Expect.equal (Ok ( Bet 100, Balance 900 ))
        , test "fails to make a over-the-budget bets" <|
            \() ->
                makeBet (Balance 1300) 1500
                    |> Expect.equal (Err NotEnoughAmount)
        ]


payoutTests : Test
payoutTests =
    describe "Player Wins"
        [ test "When both dice are the same" <|
            \() ->
                Bet 1000
                    |> Benzino.determinePayout ( Yek, Yek )
                    |> Expect.equal (ReturnToPlayer 6000)
        ]
