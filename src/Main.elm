module Main exposing (..)

import Account exposing (Account(..))
import Benzino exposing (DeterminedEvent, SessionAggregates, SessionContext)
import Browser
import Chart as C
import Chart.Attributes as CA
import Common.Die exposing (Face(..), glyphFor)
import Common.Money exposing (Money)
import Debug exposing (toString)
import Element
import Element.Border
import Element.Font
import Element.Input
import History
import Html exposing (Html)
import List.Extra
import Random



-- MAIN


main =
    Browser.element
        { init = initContext
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }



-- MODEL


type alias Model =
    SessionContext


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


initContext : () -> ( SessionContext, Cmd Msg )
initContext _ =
    ( ( { history = History.empty
        , account = Account 3000
        }
      , Random.initialSeed 0
      )
    , Cmd.none
    )


rollResultsDisplay : List DeterminedEvent -> Element.Element Msg
rollResultsDisplay history =
    let
        xxlSize =
            200

        pictogramFor : ( Face, Face ) -> Element.Element Msg
        pictogramFor ( rolledA, rolledB ) =
            Element.row
                [ Element.Font.size xxlSize
                ]
                [ Element.text (glyphFor rolledA)
                , Element.text (glyphFor rolledB)
                ]
    in
    case History.last history of
        Nothing ->
            pictogramFor ( Shesh, Yek )

        Just { roll } ->
            pictogramFor roll


displayBenzinoScene : { a | account : Account, history : List DeterminedEvent } -> Element.Element Msg
displayBenzinoScene { account, history } =
    let
        balanceDisplay =
            Element.text ("Account Balance: " ++ toString (Account.balanceOf account))

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
    Element.column []
        [ Element.el
            [ Element.alignRight ]
            balanceDisplay
        , rollResultsDisplay history
        , rollTrigger
        ]


statsChart : SessionAggregates -> Element.Element Msg
statsChart { history, account } =
    let
        reHistory =
            List.reverse history

        reAccounts =
            reHistory
                |> List.Extra.scanr
                    (\{ bet, payout } dirtyBalance ->
                        dirtyBalance + bet - payout
                    )
                    (Account.balanceOf account)

        reAll =
            List.Extra.zip reHistory reAccounts

        scores =
            reAll
                |> List.indexedMap
                    (\idx ( event, balance ) ->
                        { idx = toFloat idx
                        , bet = toFloat event.bet
                        , payout = toFloat event.payout
                        , balance = toFloat balance
                        }
                    )

        optimalHeight =
            400

        optimalWidth =
            120 + (history |> List.length) * 12

        chart =
            C.chart
                [ CA.height optimalHeight
                , CA.width (toFloat optimalWidth)
                ]
                [ C.xLabels []
                , C.yLabels [ CA.withGrid ]
                , C.series .idx
                    [ C.interpolated .balance
                        [ CA.stepped, CA.opacity 0.2 ]
                        [ CA.borderWidth 2, CA.border "white" ]
                    ]
                    scores
                , C.bars
                    [ CA.x1 .idx ]
                    [ C.bar .bet []
                    , C.bar .payout []
                    ]
                    scores
                ]
    in
    Element.el
        [ Element.width (Element.px optimalWidth)
        , Element.padding 50
        ]
        (Element.html chart)


view : Model -> Html Msg
view ( aggregates, _ ) =
    Html.div []
        [ Element.layout [ Element.padding 50 ]
            (Element.row
                []
                [ displayBenzinoScene aggregates
                , statsChart aggregates
                ]
            )
        ]
