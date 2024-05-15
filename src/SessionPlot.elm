module SessionPlot exposing (..)

import Account
import Chart as C
import Chart.Attributes as CA
import Element exposing (..)
import List.Extra
import Session exposing (SessionState)


type AccountPointState
    = Settled
    | Won
    | Lost


plotSession : SessionState e -> Element msg
plotSession currentState =
    let
        accountFlow =
            Session.balanceSettledThrough currentState

        reMid =
            List.Extra.zip currentState.history accountFlow
                |> List.map
                    (\( event, balance ) ->
                        if event.payout < event.bet then
                            { balance = balance
                            , status = Lost
                            }

                        else
                            { balance = balance
                            , status = Won
                            }
                    )

        reAcc =
            accountFlow
                |> List.map (\nthAccount -> { balance = nthAccount, status = Settled })

        reAll =
            List.Extra.interweave reAcc reMid
                |> List.indexedMap
                    (\idx { balance, status } ->
                        { balance = toFloat balance
                        , status = status
                        , idx = toFloat idx
                        }
                    )

        optimalWidth =
            ((currentState.history |> List.length) + 1) * 20

        chart =
            C.chart
                [ CA.height 360
                , CA.width (toFloat optimalWidth)
                ]
                [ C.yLabels [ CA.amount 1, CA.withGrid ]
                , C.series .idx
                    [ C.interpolated .balance
                        [ CA.monotone, CA.width 2 ]
                        []
                    ]
                    (reAll |> List.filter (\{ status } -> status == Settled))
                , C.series .idx
                    [ C.scatter .balance []
                        |> C.variation
                            (\i data ->
                                if i == 0 then
                                    [ CA.circle, CA.color CA.blue ]

                                else
                                    case data.status of
                                        Won ->
                                            [ CA.plus, CA.color CA.green ]

                                        Lost ->
                                            [ CA.cross, CA.color CA.red ]

                                        _ ->
                                            [ CA.circle, CA.color CA.purple ]
                            )
                    ]
                    reAll
                ]
    in
    Element.el
        [ Element.width (Element.px optimalWidth)
        , Element.centerX
        ]
        (Element.html chart)
