module Superigra.View exposing (..)

import Element exposing (..)
import Element.Events exposing (onClick)
import Element.Font
import Element.Input
import Random exposing (..)
import Superigra.Card exposing (..)
import Superigra.Superigra as Superigra exposing (..)


cardBack =
    "🂠"


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
                    (FaceUp dealerCard :: (playerChoices |> List.map FaceDown))
                        |> List.map (cardElement selectCard)

                Resolved dealerCard playerCard playerChoices ->
                    (dealerCard :: playerChoices)
                        |> List.map FaceUp
                        |> List.map (cardElement selectCard)

                _ ->
                    []
    in
    Element.row
        [ Element.spacing 8
        , Element.Font.size 64
        ]
        cardsOnTable


freshDeckElement : msg -> Element msg
freshDeckElement dealRound =
    Element.Input.button
        [ Element.centerX
        , Element.padding 8
        ]
        { label = text cardBack
        , onPress = Just dealRound
        }


cardElement : (Card -> msg) -> CardInTheGame -> Element msg
cardElement selectCard cardInGame =
    let
        ( glyph, handler ) =
            case cardInGame of
                FaceUp card ->
                    ( card |> cardToUnicode
                    , Nothing
                    )

                FaceDown card ->
                    ( cardBack
                    , Just (selectCard card)
                    )
    in
    Element.Input.button
        [ Element.centerX
        , Element.padding 8
        ]
        { label = text glyph
        , onPress = handler
        }
