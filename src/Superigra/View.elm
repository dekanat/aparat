module Superigra.View exposing (..)

import Element exposing (text)
import Element.Font
import Random exposing (..)
import Superigra.Card exposing (..)
import Superigra.Superigra as Superigra exposing (..)


cardBack =
    "🂠"


unrevealedCard =
    Element.text cardBack


view =
    let
        cardsOnTable =
            Superigra.dealCards 5 |> List.map cardElement
    in
    Element.row [ Element.spacing 15, Element.Font.size 72 ]
        cardsOnTable


cardElement : CardInTheGame -> Element.Element msg
cardElement cardInGame =
    case cardInGame of
        FaceUp card ->
            card |> cardToUnicode |> text

        FaceDown _ ->
            cardBack |> text
