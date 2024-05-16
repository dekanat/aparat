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
    | Sliding ( BalancePoint, BalancePoint )


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


scatterIndicators plottedBalance =
    let
        midpointX ( a, b ) =
            (a.idx + b.idx) / 2

        markOnFixedY =
            C.scatter (always 0) []
                |> C.variation
                    (\_ ( a, b ) ->
                        if a.balance < b.balance then
                            [ CA.plus, CA.color CA.mint ]

                        else
                            [ CA.cross, CA.color CA.pink ]
                    )
    in
    plottedBalance
        |> slidingPair
        |> C.seriesMap Sliding midpointX [ markOnFixedY ]


delineateBalanceFlow =
    C.seriesMap Settled
        .idx
        [ C.interpolated .balance
            [ CA.monotone, CA.width 2 ]
            [ CA.circle ]
        ]


takeRight : Int -> List a -> List a
takeRight n list =
    let
        card =
            List.length list
    in
    if card > n then
        list
            |> List.drop (card - n)

    else
        list


plotSession : SessionState e -> Element msg
plotSession currentState =
    let
        balanceFlow =
            currentState
                |> Session.balanceSettledThrough

        plottedBalance =
            balanceFlow
                |> List.indexedMap plotBalancePoint

        unitSize =
            20

        fullHeight =
            unitSize * 20

        fullWidth =
            List.length balanceFlow * unitSize

        chart =
            C.chart
                [ CA.height (toFloat fullHeight)
                , CA.width (toFloat fullWidth)
                ]
                [ C.yLabels [ CA.amount 1, CA.withGrid ]
                , delineateBalanceFlow plottedBalance
                , scatterIndicators plottedBalance
                ]
    in
    Element.el
        [ Element.width (Element.px fullWidth)
        , Element.alignRight
        ]
        (Element.html chart)
