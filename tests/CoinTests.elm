module CoinTests exposing (..)

import Common.Money exposing (Money)
import Expect
import Fuzz exposing (int)
import Random
import Round exposing (Round)
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


calculatePayout : Money -> Face -> Money
calculatePayout bet outcome =
    case outcome of
        Xush ->
            bet * 2

        _ ->
            0


type alias GameOfChance e =
    { generator : Random.Generator e
    , calculate : Money -> e -> Money
    }


type alias RoundOutcome e =
    { event : e
    , payout : Money
    }


friendlyCoinGame : GameOfChance Face
friendlyCoinGame =
    { generator = fairCoinFlip
    , calculate = calculatePayout
    }


playOnce : GameOfChance e -> Money -> Random.Seed -> ( RoundOutcome e, Random.Seed )
playOnce { generator, calculate } bet seed =
    let
        ( event, nextSeed ) =
            Random.step generator seed

        payout =
            calculate bet event
    in
    ( RoundOutcome event payout, nextSeed )
