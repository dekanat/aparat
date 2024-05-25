module Superigra.View exposing (..)

import Element exposing (text)
import Element.Font
import Random exposing (..)
import Superigra.Card exposing (..)
import Superigra.Deck as Deck exposing (Deck, freshDeck)


cardBack =
    "🂠"


unrevealedCard =
    Element.text cardBack


view =
    let
        ( dealtCards, _ ) =
            Deck.shuffleAndDeal 5 freshDeck

        cards =
            case dealtCards of
                open :: closed ->
                    { card = open, revealed = True }
                        :: (closed |> List.map (\cc -> { card = cc, revealed = False }))

                _ ->
                    []

        cardsOnTable =
            cards |> List.map cardElement
    in
    Element.row [ Element.spacing 15, Element.Font.size 72 ]
        cardsOnTable


type alias CardInTheGame =
    { card : Card
    , revealed : Bool
    }


cardElement { card, revealed } =
    if revealed then
        card |> cardToUnicode |> text

    else
        cardBack |> text
