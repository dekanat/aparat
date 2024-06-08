module Control.View exposing (..)

import Common.Money exposing (Money)
import Control.Account as Account
import Control.Control as Control
import Debug exposing (toString)
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


type alias Translate msg =
    Control.Request -> msg


view : Translate msg -> Control.State -> Element.Element msg
view translate { account, selectedBet } =
    Element.row
        [ Element.spaceEvenly, Element.width (Element.px 520) ]
        [ Element.text ("Account Balance: " ++ toString (account |> Account.balanceOf))
        , Element.Input.button
            [ Element.centerX
            , Element.Border.width 2
            , Element.Border.rounded 2
            , Element.padding 8
            , Element.alignRight
            ]
            { label = Element.text ("Roll for " ++ toString selectedBet)
            , onPress = selectedBet |> Maybe.map (translate << Control.Bet)
            }
        ]
