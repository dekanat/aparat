module ControlPanel.BetControl exposing (..)

import Common.Money exposing (Money)
import Element
import Element.Border
import Element.Input


type alias Callback msg =
    { orderBet : Money -> msg
    }


betControl : Callback msg -> () -> Element.Element msg
betControl { orderBet } _ =
    Element.Input.button
        [ Element.centerX
        , Element.Border.width 2
        , Element.Border.rounded 2
        , Element.padding 8
        ]
        { label = Element.text "Roll for 1000"
        , onPress = Just (orderBet 1000)
        }
