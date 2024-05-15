module SessionPlot exposing (..)

import Chart as C
import Chart.Attributes as CA
import Common.Helpers exposing (slidingWindow)
import Element exposing (..)
import Session exposing (SessionState)


plotBalancePoint : Int -> Int -> { idx : Float, balance : Float }
plotBalancePoint idx balance =
    { idx = toFloat idx
    , balance = toFloat balance
    }


type PlotElement
    = BalanceLine { idx : Float, balance : Float }
    | ResultIdentificator { idx : Float, isWin : Bool }


plotSession : SessionState e -> Element msg
plotSession currentState =
    let
        balanceFlow =
            currentState
                |> Session.balanceSettledThrough

        plottedBalanceFlow =
            balanceFlow
                |> List.indexedMap plotBalancePoint

        plottedWinLose =
            plottedBalanceFlow
                |> slidingWindow 2
                |> List.foldl
                    (\window acc ->
                        case window of
                            [ a, b ] ->
                                { idx = a.idx + (b.idx - a.idx) / 2
                                , isWin = b.balance > a.balance
                                }
                                    :: acc

                            _ ->
                                acc
                    )
                    []

        scatterIdentificators =
            C.seriesMap ResultIdentificator
                .idx
                [ C.scatter (\_ -> 0) []
                    |> C.variation
                        (\_ w ->
                            if w.isWin then
                                [ CA.plus, CA.color CA.mint ]

                            else
                                [ CA.cross, CA.color CA.pink ]
                        )
                ]
                plottedWinLose

        optimalWidth =
            List.length balanceFlow * 20

        chart =
            C.chart
                [ CA.height 360
                , CA.width (toFloat optimalWidth)
                ]
                [ C.yLabels [ CA.amount 1, CA.withGrid ]
                , C.seriesMap BalanceLine
                    .idx
                    [ C.interpolated .balance
                        [ CA.monotone, CA.width 2 ]
                        [ CA.circle ]
                    ]
                    plottedBalanceFlow
                , scatterIdentificators
                ]
    in
    Element.el
        [ Element.width (Element.px optimalWidth)
        , Element.centerX
        ]
        (Element.html chart)
