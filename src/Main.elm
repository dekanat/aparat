module Main exposing (..)

import Account exposing (Account(..))
import Benzino
import Browser
import Char exposing (isHexDigit)
import Chart as C
import Chart.Attributes as CA
import Common.Die exposing (Face(..), glyphFor)
import Common.Money exposing (Money)
import Debug exposing (toString)
import Element
import Element.Border
import Element.Font
import Element.Input
import History exposing (History)
import Html exposing (Html)
import List.Extra
import Random
import Session exposing (Session, SessionState)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }



-- MODEL


type alias Model =
    Session Benzino.RoundDetails


type Msg
    = PlayerWantsToBet Money


update : Msg -> Model -> ( Model, Cmd Msg )
update msg session =
    case msg of
        PlayerWantsToBet moneyToBet ->
            case Benzino.playOnce moneyToBet session of
                Ok ctx ->
                    ( ctx
                    , Cmd.none
                    )

                Err _ ->
                    ( session, Cmd.none )



-- VIEW


init : () -> ( Model, Cmd Msg )
init _ =
    ( ( { history = History.empty
        , account = Account 3000
        }
      , Random.initialSeed 0
      )
    , Cmd.none
    )


rollResultsDisplay : History Benzino.RoundDetails -> Element.Element Msg
rollResultsDisplay history =
    let
        xxlSize =
            200

        pictogramFor : ( Face, Face ) -> Element.Element Msg
        pictogramFor ( rolledA, rolledB ) =
            Element.row
                [ Element.Font.size xxlSize
                , Element.spacing 8
                ]
                [ Element.text (glyphFor rolledA)
                , Element.text (glyphFor rolledB)
                ]
    in
    case History.last history of
        Nothing ->
            pictogramFor ( Shesh, Yek )

        Just { details } ->
            pictogramFor details


displayBenzinoScene : SessionState Benzino.RoundDetails -> Element.Element Msg
displayBenzinoScene { account, history } =
    let
        balanceDisplay =
            case account of
                Account balance ->
                    Element.text ("Account Balance: " ++ toString balance)

        rollTrigger =
            Element.Input.button
                [ Element.centerX
                , Element.Border.width 2
                , Element.Border.rounded 2
                , Element.padding 8
                ]
                { label = Element.text "Roll for 1000"
                , onPress = Just (PlayerWantsToBet 1000)
                }
    in
    Element.column
        [ Element.width Element.fill
        , Element.centerX
        , Element.Border.width 2
        , Element.Border.rounded 2
        , Element.padding 32
        , Element.spacing 8
        ]
        [ Element.el
            [ Element.alignRight ]
            balanceDisplay
        , Element.el
            [ Element.centerX ]
            (rollResultsDisplay history)
        , rollTrigger
        ]


view : Model -> Html Msg
view ( aggregates, _ ) =
    Element.layout [] <|
        Element.row
            [ Element.width Element.fill
            , Element.centerY
            , Element.spaceEvenly
            , Element.spacing 30
            ]
            [ displayCharts aggregates
            , displayBenzinoScene aggregates
            , Element.el [ Element.width Element.fill ] Element.none
            ]


type AccountPointState
    = Settled
    | Won
    | Lost


displayCharts : SessionState Benzino.RoundDetails -> Element.Element Msg
displayCharts { account, history } =
    let
        accountFlow =
            history
                |> List.Extra.scanr
                    (\{ bet, payout } dirtyBalance ->
                        dirtyBalance + bet - payout
                    )
                    (Account.balanceOf account)

        reMid =
            List.Extra.zip history accountFlow
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
            ((history |> List.length) + 1) * 20

        chart =
            C.chart
                [ CA.height 360
                , CA.width (toFloat optimalWidth)
                ]
                [ C.xLabels [ CA.amount 2, CA.hideOverflow ]
                , C.yLabels [ CA.amount 3, CA.withGrid ]
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
        [ Element.width Element.fill
        , Element.height (Element.px 360)
        ]
        (Element.el
            [ Element.width (Element.px optimalWidth)
            , Element.alignRight
            ]
            (Element.html chart)
        )
