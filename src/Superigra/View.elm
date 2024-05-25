module Superigra.View exposing (..)

import Element exposing (text)
import Element.Font
import List.Extra
import Superigra.Card as Card
import Superigra.Deck


view =
    Element.column [ Element.spacing 15 ]
        (Superigra.Deck.deck
            |> List.map
                (Card.cardToUnicode >> Element.text)
            |> List.Extra.groupsOf 13
            |> List.map
                (Element.row
                    [ Element.spacing 8
                    , Element.Font.size 27
                    ]
                )
        )
