module AparatTests exposing (..)

import Aparat exposing (playRound, resolvePayout)
import Common.Die exposing (Face(..))
import Debug exposing (toString)
import Expect exposing (..)
import Fuzz exposing (..)
import Random exposing (initialSeed)
import Set
import Test exposing (..)


aparatTests : Test
aparatTests =
    describe "Aparat"
        [ describe "Round"
            [ fuzz2 int int "should pay based on roll outcome" <|
                \salt amountToBet ->
                    let
                        currentSeed =
                            initialSeed salt

                        ( result, _ ) =
                            amountToBet
                                |> playRound currentSeed

                        expectedPayoutFor =
                            resolvePayout amountToBet
                    in
                    result.payout
                        |> Expect.equal (expectedPayoutFor result.roll)
            , fuzz int "should vary outcome like a normal die" <|
                \salt ->
                    let
                        currentSeed =
                            initialSeed salt

                        fixedBet =
                            500

                        itr _ ( previousOutcomes, seed ) =
                            let
                                ( { roll }, nextSeed ) =
                                    playRound seed fixedBet
                            in
                            ( roll :: previousOutcomes, nextSeed )

                        results =
                            List.range 1 1000
                                |> List.foldl itr ( [], currentSeed )
                                |> Tuple.first
                    in
                    results
                        |> List.map toString
                        |> Set.fromList
                        |> Set.size
                        |> Expect.equal 36
            ]
        , describe "Determine Payout"
            [ test "that apprat notifies of wins" <|
                \() ->
                    Aparat.resolvePayout 100 ( Panj, Panj )
                        |> Expect.equal 600
            , test "that apprat notifies of lose" <|
                \() ->
                    Aparat.resolvePayout 100 ( Panj, Yek )
                        |> Expect.equal 0
            ]
        ]
