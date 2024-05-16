module CoinTests exposing (..)

import Expect
import Fuzz exposing (int)
import Random
import Test exposing (Test, describe)


type Face
    = Gir
    | Xush


fairCoinFlip : Random.Generator Face
fairCoinFlip =
    Random.uniform Gir [ Xush ]


fairCoinTests : Test
fairCoinTests =
    describe "Fair Coin"
        [ Test.skip <|
            describe "On the long run"
                [ Test.fuzz int "Heads and tails are equally likely to occur" <|
                    \salt ->
                        let
                            firstSeed =
                                Random.initialSeed salt

                            longRun =
                                1000000

                            updateStats { heads, tails } outcome =
                                case outcome of
                                    Gir ->
                                        { heads = heads + 1, tails = tails }

                                    Xush ->
                                        { heads = heads, tails = tails + 1 }

                            stats =
                                List.range 1 longRun
                                    |> List.foldl
                                        (\_ ( earlierStats, currentSeed ) ->
                                            currentSeed
                                                |> Random.step fairCoinFlip
                                                |> Tuple.mapFirst (updateStats earlierStats)
                                        )
                                        ( { heads = 0, tails = 0 }, firstSeed )
                                    |> Tuple.first
                        in
                        (toFloat stats.heads / toFloat stats.tails)
                            |> Expect.within (Expect.Absolute 0.01) 1
                ]
        ]
