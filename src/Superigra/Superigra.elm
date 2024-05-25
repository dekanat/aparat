module Superigra.Superigra exposing (..)

import Superigra.Card exposing (Card)
import Superigra.Deck as Deck


type CardInTheGame
    = FaceUp Card
    | FaceDown Card


dealCards size =
    let
        ( cardsSelected, _ ) =
            Deck.shuffleAndDeal size Deck.freshDeck

        handsDealt =
            case cardsSelected of
                dealer :: player ->
                    FaceUp dealer :: (player |> List.map FaceDown)

                _ ->
                    []
    in
    handsDealt
