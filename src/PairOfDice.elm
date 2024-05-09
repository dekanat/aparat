module PairOfDice exposing (..)

import Die exposing (Face, rollingDie)
import Random


type alias RollOutcome =
    ( Face, Face )


rollingPairOfDice : Random.Generator RollOutcome
rollingPairOfDice =
    Random.pair rollingDie rollingDie
