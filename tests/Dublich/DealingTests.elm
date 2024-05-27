module Dublich.DealingTests exposing (..)

import Dublich.Deck exposing (dealHandFromTop)
import Expect
import List.Extra
import Random
import Test exposing (Test, describe, test)


testDealing : Test
testDealing =
    let
        choicesCount =
            4

        experiment =
            Random.initialSeed
                >> Random.step (dealHandFromTop choicesCount)
                >> Tuple.first

        hasUniqueLength ( openCard, playerChoices ) =
            (openCard :: playerChoices)
                |> (List.Extra.unique >> List.length)
                |> Expect.equal

        manyAttempts =
            List.Extra.initialize 100 experiment
    in
    describe "generator"
        [ test "doesn't include same card twice in a single hand" <|
            \() ->
                (choicesCount + 1)
                    |> Expect.all (manyAttempts |> List.map hasUniqueLength)
        ]
