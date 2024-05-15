module SessionPlot exposing (..)

import Chart as C
import Chart.Attributes as CA exposing (window)
import Common.Helpers exposing (slidingWindow)
import Element exposing (..)
import Session exposing (SessionState)


plotBalancePoint : Int -> Int -> { idx : Float, balance : Float }
plotBalancePoint idx balance =
    { idx = toFloat idx
    , balance = toFloat balance
    }


type alias BalancePoint =
    { idx : Float
    , balance : Float
    }


type PlotElement
    = Settled BalancePoint
    | ResultIdentificator { idx : Float, isWin : Bool }


slidingPair : List a -> List ( a, a )
slidingPair list =
    list
        |> slidingWindow 2
        |> List.filterMap
            (\window ->
                case window of
                    [ a, b ] ->
                        Just ( a, b )

                    _ ->
                        Nothing
            )


plotIdentificators plottedBalanceFlow =
    let
        plottedWinLose =
            plottedBalanceFlow
                |> slidingPair
                |> List.map
                    (\( a, b ) ->
                        { idx = a.idx + (b.idx - a.idx) / 2
                        , isWin = b.balance > a.balance
                        }
                    )

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
    in
    scatterIdentificators plottedWinLose


plotSession : SessionState e -> Element msg
plotSession currentState =
    let
        balanceFlow =
            currentState
                |> Session.balanceSettledThrough

        plottedBalanceFlow =
            balanceFlow
                |> List.indexedMap plotBalancePoint

        optimalWidth =
            List.length balanceFlow * 20

        chart =
            C.chart
                [ CA.height 360
                , CA.width (toFloat optimalWidth)
                ]
                [ C.yLabels [ CA.amount 1, CA.withGrid ]
                , C.seriesMap Settled
                    .idx
                    [ C.interpolated .balance
                        [ CA.monotone, CA.width 2 ]
                        [ CA.circle ]
                    ]
                    plottedBalanceFlow
                , plotIdentificators plottedBalanceFlow
                ]
    in
    Element.el
        [ Element.width (Element.px optimalWidth)
        , Element.centerX
        ]
        (Element.html chart)
