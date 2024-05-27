module Dublich.Deck exposing (..)

import Dublich.Card exposing (Card(..), regularCards)
import List.Extra
import Random
import Random.List


type alias Deck =
    List Card


randomChoices : Int -> List a -> Random.Generator (List a)
randomChoices count list =
    list
        |> Random.List.choices count
        |> Random.map Tuple.first


dealHandFromTop : Int -> Random.Generator ( Card, List Card )
dealHandFromTop count =
    let
        choicesToBeatGiven : Card -> Random.Generator ( Card, List Card )
        choicesToBeatGiven dealerCard =
            let
                deckWithoutReplacement =
                    (Joker :: regularCards)
                        |> List.Extra.remove dealerCard
            in
            deckWithoutReplacement
                |> randomChoices count
                |> Random.map (Tuple.pair dealerCard)
    in
    Random.uniform Joker regularCards
        |> Random.andThen choicesToBeatGiven
