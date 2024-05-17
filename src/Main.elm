module Main exposing (..)

import Account exposing (Account(..))
import Benzino.Benzino
import Benzino.View
import Browser
import Common.Money exposing (Money)
import Debug exposing (toString)
import Element
import Element.Border
import Element.Input
import History
import Html exposing (Html)
import Medium
import Random
import Session exposing (Session, SessionState)
import SessionPlot exposing (plotSession)



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
    Session Benzino.Benzino.DiceRoll


type Msg
    = PlayerWantsToBet Money


update : Msg -> Model -> ( Model, Cmd Msg )
update msg session =
    case msg of
        PlayerWantsToBet moneyToBet ->
            case Medium.playOnce moneyToBet session of
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


displayGameScene : SessionState Benzino.Benzino.DiceRoll -> Element.Element Msg
displayGameScene { account, history } =
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
        [ Element.width (Element.px 520)
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
            (History.last history
                |> Maybe.map .details
                |> Benzino.View.benzinoResultsDisplay { size = 200 }
            )
        , rollTrigger
        ]


view : Model -> Html Msg
view ( aggregates, _ ) =
    Element.layout [] <|
        Element.column
            [ Element.width Element.fill
            , Element.centerX
            , Element.centerY
            , Element.spaceEvenly
            , Element.spacing 48
            ]
            [ displayGameScene aggregates
            , Element.el
                [ Element.width Element.fill
                , Element.padding 64
                ]
                (plotSession aggregates)
            ]
