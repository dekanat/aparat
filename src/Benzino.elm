module Benzino exposing (..)

import Account exposing (Account(..))
import Aparat exposing (DiceRoll)
import Common.Money exposing (Money)
import Random


type alias RoundDetails =
    DiceRoll


type alias RoundOutcome e =
    { event : e
    , payout : Money
    }


playRound : Money -> Random.Seed -> ( RoundOutcome DiceRoll, Random.Seed )
playRound bet seed =
    seed
        |> Random.step Aparat.rollingPairOfDice
        |> Tuple.mapFirst
            (\event ->
                { event = event
                , payout = Aparat.calculatePayout bet event
                }
            )
