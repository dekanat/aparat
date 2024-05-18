module Benzino.RandomDeviceTests exposing (..)

import Account exposing (Account(..), AccountingProblem(..))
import Aparat exposing (rollingPairOfDice)
import Benzino exposing (..)
import Common.Die exposing (Face(..))
import Common.Money exposing (Money)
import Expect exposing (..)
import History
import Random
import Result exposing (..)
import Session exposing (Session)
import Set
import Test exposing (..)
import Time exposing (Weekday(..))


compoundTest : Test
compoundTest =
    describe "Random Device fo benzino"
        [ test "Should reveal all possible combinations on the long run" <|
            \() ->
                let
                    randomLoop _ ( events, currentSeed ) =
                        let
                            ( event, nextSeed ) =
                                Random.step rollingPairOfDice currentSeed
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
