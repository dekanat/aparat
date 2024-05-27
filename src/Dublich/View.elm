module Dublich.View exposing (..)

import Dublich.Card exposing (..)
import Dublich.CardGlyph exposing (cardBackSymbol, cardSymbol)
import Dublich.Dublich as Dublich exposing (..)
import Element exposing (..)
import Element.Font
import Element.Input
import Random exposing (..)


type alias CallbackInterface msg =
    { toSelf : Request -> msg
    , toSelfSeeded : (Random.Seed -> Dublich.Request) -> msg
    }


view : CallbackInterface msg -> Dublich.State -> Element msg
view { toSelfSeeded, toSelf } state =
    let
        cardsOnTable =
            case state of
                Initial _ ->
                    [ freshDeckElement toSelfSeeded ]

                Proposed dealerCard playerChoices ->
                    viewDealt toSelf dealerCard playerChoices

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


viewDealt : (Request -> msg) -> Card -> List Card -> List (Element msg)
viewDealt trigger dealer playerChoices =
    let
        dealerCard =
            dealer |> neutralCard

        concealedChoices =
            playerChoices
                |> List.map
                    (\card ->
                        cardControl
                            (Just (trigger (SelectCard card)))
                            cardBackSymbol
                    )
    in
    dealerCard :: concealedChoices


viewResolved : Card -> Card -> List Card -> List (Element msg)
viewResolved dealer _ presentedChoices =
    (dealer :: presentedChoices)
        |> List.map neutralCard


freshDeckElement : ((Random.Seed -> Request) -> msg) -> Element msg
freshDeckElement randomize =
    cardControl (Just (randomize DealCards)) cardBackSymbol
