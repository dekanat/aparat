module AparatTests exposing (..)

import Aparat exposing (Payout(..), determine)
import Benzino exposing (Bet(..), RollOutcome, rollingPairOfDice)
import Common.Die exposing (Face(..))
import Debug exposing (toString)
import Expect exposing (..)
import Fuzz exposing (..)
import List
import Random exposing (Seed, initialSeed)
import Set
import Test exposing (..)


type alias Ban =
    { payout : Payout
    , rollOutcome : RollOutcome
    }


playRound : Seed -> Bet -> ( Ban, Seed )
playRound seed bet =
    let
        ( rollOutcome, newSeed ) =
            Random.step rollingPairOfDice seed

        payout =
            determine bet rollOutcome
    in
    ( { payout = payout, rollOutcome = rollOutcome }, newSeed )


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
                            Bet amountToBet
                                |> playRound currentSeed

                        expectedPayoutFor =
                            determine (Bet amountToBet)
                    in
                    result.payout
                        |> Expect.equal (expectedPayoutFor result.rollOutcome)
            , fuzz int "should vary outcome like a normal die" <|
                \salt ->
                    let
                        currentSeed =
                            initialSeed salt

                        fixedBet =
                            Bet 500

                        itr _ ( outcomes, seed ) =
                            let
                                ( { rollOutcome }, newSeed ) =
                                    playRound seed fixedBet
                            in
                            ( rollOutcome :: outcomes, newSeed )

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
                    Aparat.determine (Bet 100) ( Panj, Panj )
                        |> Expect.equal (Win 600)
            , test "that apprat notifies of lose" <|
                \() ->
                    Aparat.determine (Bet 100) ( Panj, Yek )
                        |> Expect.equal (Lose 100)
            ]
        ]
