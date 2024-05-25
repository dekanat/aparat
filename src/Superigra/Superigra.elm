module Superigra.Superigra exposing (..)

import Random
import Random.List
import Superigra.Card exposing (Card)
import Superigra.Deck as Deck


type Request
    = SelectCard Card


type CardInTheGame
    = FaceUp Card
    | FaceDown Card


dealHand : List Card -> List CardInTheGame
dealHand cardsSelected =
    case cardsSelected of
        dealer :: player ->
            FaceUp dealer :: (player |> List.map FaceDown)

        _ ->
            []


type alias State =
    List CardInTheGame


init : Random.Seed -> State
init seed =
    let
        dealFiveFromShuffled =
            Deck.freshDeck
                |> Random.List.choices 5
                |> Random.map (Tuple.first >> dealHand)

        ( cards, _ ) =
            seed
                |> Random.step dealFiveFromShuffled
    in
    cards
