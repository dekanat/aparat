module AparatTests exposing (..)

import Aparat exposing (Payout(..), determine)
import Benzino exposing (Bet(..), RollOutcome, determinePayout)
import Common.Die exposing (Face(..))
import Expect exposing (..)
import Fuzz exposing (..)
import Random exposing (Seed, initialSeed)
import Test exposing (..)


type alias Ban =
    { payout : Payout
    , rollOutcome : RollOutcome
    }


playRound : Seed -> Bet -> ( Ban, Seed )
playRound seed bet =
    let
        rollOutcome =
            ( Yek, Yek )

        payout =
            Win 3000
    in
    ( { payout = payout, rollOutcome = rollOutcome }, seed )


aparatTests : Test
aparatTests =
    describe "Aparat"
        [ describe "Round"
            [ fuzz2 int int "should play on a bet" <|
                \seedX amountToBet ->
                    let
                        currentSeed =
                            initialSeed seedX

                        ( result, _ ) =
                            Bet amountToBet
                                |> playRound currentSeed

                        expectedPayoutFor =
                            determine (Bet 500)
                    in
                    result.payout
                        |> Expect.equal (expectedPayoutFor result.rollOutcome)
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
