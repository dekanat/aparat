module SessionPlot exposing (..)

import Account
import Chart as C
import Chart.Attributes as CA
import Common.Helpers exposing (slidingPair)
import Common.Money exposing (Money)
import Element exposing (..)
import List.Extra
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


balanceSettledThrough : SessionState e -> List Money
balanceSettledThrough { history, account } =
    let
        recoverEarlier event balanceAfterEvent =
            balanceAfterEvent - event.payout + event.bet
    in
    history |> List.Extra.scanr recoverEarlier (Account.balanceOf account)


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
            balanceSettledThrough currentState

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
