module SessionPlot exposing (..)

import Chart as C
import Chart.Attributes as CA
import Common.Money exposing (Money)
import Element exposing (..)
import Session exposing (SessionState)


type alias BalancePlotPoint =
    { idx : Int
    , balance : Money
    }


plotSession : SessionState e -> Element msg
plotSession currentState =
    let
        balanceFlow =
            currentState
                |> Session.balanceSettledThrough

        optimalWidth =
            List.length balanceFlow * 20

        chart =
            C.chart
                [ CA.height 360
                , CA.width (toFloat optimalWidth)
                ]
                [ C.yLabels [ CA.amount 1, CA.withGrid ]
                , C.series (.idx >> toFloat)
                    [ C.interpolated (.balance >> toFloat)
                        [ CA.monotone, CA.width 2 ]
                        [ CA.circle ]
                    ]
                    (balanceFlow |> List.indexedMap BalancePlotPoint)
                ]
    in
    Element.el
        [ Element.width (Element.px optimalWidth)
        , Element.centerX
        ]
        (Element.html chart)
