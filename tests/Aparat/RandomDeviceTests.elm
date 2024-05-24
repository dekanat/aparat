module Aparat.RandomDeviceTests exposing (..)

import Aparat.ChanceMechanism exposing (fairPairOfDice)
import Expect
import Random
import Set
import Test exposing (..)


compoundTest : Test
compoundTest =
    describe "Random Device"
        [ describe "Pair of fair dice"
            [ test "Should reveal all possible combinations on the long run" <|
                \() ->
                    let
                        randomLoop _ ( events, currentSeed ) =
                            let
                                ( event, nextSeed ) =
                                    Random.step fairPairOfDice currentSeed
                            in
                            ( event :: events, nextSeed )

                        longSim =
                            List.range 1 1000
                                |> List.foldl randomLoop ( [], Random.initialSeed 1 )
                                |> Tuple.first

                        uniquePairs =
                            longSim
                                |> List.map Debug.toString
                                |> Set.fromList
                    in
                    uniquePairs
                        |> Set.size
                        |> Expect.equal 36
            ]
        ]
