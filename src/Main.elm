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


{-| Reduce a list from the left, building up all
of the intermediate results into a list.
-}
scanl : (a -> b -> b) -> b -> List a -> List b
scanl fn b =
    let
        scan a bs =
            case bs of
                hd :: _ ->
                    fn a hd :: bs

                _ ->
                    []
    in
    List.foldl scan [ b ] >> List.reverse


statsChart : SessionAggregates -> Element.Element Msg
statsChart { history, account } =
    let
        datax =
            history
                |> List.reverse
                |> List.Extra.scanr
                    (\{ bet, payout } dirtyBalance ->
                        dirtyBalance + bet - payout
                    )
                    (Account.balanceOf account)

        _ =
            history
                |> List.indexedMap
                    (\idx { bet, payout } ->
                        { x = toFloat idx
                        , bet = toFloat -bet
                        , payout = toFloat payout
                        }
                    )

        chart =
            C.chart
                [ CA.height 300
                , CA.width 300
                ]
                [ C.xLabels [ CA.withGrid, CA.ints ]
                , C.yLabels [ CA.withGrid ]
                , C.bars
                    [ CA.x1 .x, CA.ungroup ]
                    [ C.bar .balance [ CA.striped [] ]
                    , C.bar .balance [ CA.striped [] ]
                    ]
                    (datax
                        |> List.indexedMap (\idx balance -> { x = toFloat idx, balance = toFloat balance })
                    )
                ]
    in
    Element.el
        [ Element.width (Element.px 300)
        , Element.padding 50
        ]
        (Element.html chart)


view : Model -> Html Msg
view ( aggregates, _ ) =
    Html.div []
        [ Element.layout [ Element.padding 50 ]
            (Element.row []
                [ displayBenzinoScene aggregates
                , statsChart aggregates
                ]
            )
        ]
