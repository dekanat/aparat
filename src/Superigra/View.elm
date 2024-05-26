module Superigra.View exposing (..)

import Element exposing (..)
import Element.Font
import Element.Input
import Random exposing (..)
import Superigra.Card exposing (..)
import Superigra.CardGlyph exposing (cardBackSymbol, cardSymbol)
import Superigra.Superigra as Superigra exposing (..)


type alias CallbackInterface msg =
    { dealHand : msg
    , selectCard : Card -> msg
    }


view : CallbackInterface msg -> Superigra.State -> Element msg
view { dealHand, selectCard } state =
    let
        cardsOnTable =
            case state of
                Initial _ ->
                    [ freshDeckElement dealHand ]

                Proposed dealerCard playerChoices ->
                    viewDealt selectCard dealerCard playerChoices

                Resolved dealerCard playerCard playerChoices ->
                    viewResolved dealerCard playerCard playerChoices
    in
    Element.row
        []
        cardsOnTable


cardControl handler glyph =
    Element.Input.button
        [ Element.centerX
        , Element.padding 8
        , Element.Font.size 64
        ]
        { label = text glyph
        , onPress = handler
        }


neutralCard =
    cardSymbol >> cardControl Nothing


viewDealt : (Card -> msg) -> Card -> List Card -> List (Element msg)
viewDealt select dealer playerChoices =
    let
        dealerCard =
            dealer |> neutralCard

        concealedChoices =
            playerChoices
                |> List.map (\card -> cardControl (Just (select card)) cardBackSymbol)
    in
    dealerCard :: concealedChoices


viewResolved : Card -> Card -> List Card -> List (Element msg)
viewResolved dealer player presentedChoices =
    (dealer :: presentedChoices)
        |> List.map neutralCard


freshDeckElement : msg -> Element msg
freshDeckElement dealRound =
    cardControl (Just dealRound) cardBackSymbol
