module BenzinoTests exposing (..)

import Balance exposing (Balance(..))
import Benzino exposing (Bet(..), RoundOutcome(..), makeBet)
import Common.Die exposing (Face(..))
import Expect exposing (..)
import Result exposing (..)
import Test exposing (..)


betTests : Test
betTests =
    describe "makeBet"
        [ test "successfully makes a bet" <|
            \() ->
                Balance 1000
                    |> makeBet 100
                    |> Expect.equal
                        (Ok
                            ( Bet 100
                            , Balance 900
                            )
                        )
        , test "fails to make a over-the-budget bets" <|
            \() ->
                Balance 1300
                    |> makeBet 1500
                    |> Expect.err
        ]



-- payoutTests : Test
-- payoutTests =
--     describe "Player Wins"
--         [ test "When both dice are the same" <|
--             \() ->
--                 Bet 1000
--                     |> Benzino.determinePayout ( Yek, Yek )
--                     |> Expect.equal (ReturnToPlayer 6000)
--         ]
