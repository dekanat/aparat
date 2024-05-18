module Main exposing (..)

import Account exposing (Account(..))
import Benzino
import Browser
import Common.Die exposing (Face(..), glyphFor)
import Common.Money exposing (Money)
import Debug exposing (toString)
import Element
import Element.Border
import Element.Font
import Element.Input
import History exposing (History)
import Html exposing (Html)
import Random
import Session exposing (Sess(..), SessionProblem(..))
import SessionPlot exposing (plotSession)
import Task exposing (..)
import Time exposing (..)



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
    Sess Benzino.RoundDetails


type Msg
    = PlayerSubmittedBet Money
    | SessionInitiated Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg session =
    case ( session, msg ) of
        ( LoadingSession, SessionInitiated time ) ->
            let
                firstSeed =
                    Random.initialSeed (Time.posixToMillis time)

                freshSession =
                    CurrentSession
                        { lastEvent = Nothing
                        , account = Account 10000
                        }
                        firstSeed
            in
            ( freshSession, Cmd.none )

        ( CurrentSession state seed, PlayerSubmittedBet moneyToBet ) ->
            case state.account |> Account.deduct moneyToBet of
                Ok reducedAccount ->
                    let
                        ( outcome, nextSeed ) =
                            Benzino.playRound moneyToBet seed

                        settledAccount =
                            reducedAccount |> Account.add outcome.payout
                    in
                    ( CurrentSession
                        { state
                            | account = settledAccount
                            , lastEvent = Just outcome.event
                        }
                        nextSeed
                    , Cmd.none
                    )

                Err _ ->
                    ( session, Cmd.none )

        _ ->
            ( LoadingSession, Cmd.none )


initiateSession : Cmd Msg
initiateSession =
    Task.perform SessionInitiated Time.now



-- VIEW


init : () -> ( Model, Cmd Msg )
init _ =
    ( LoadingSession
    , initiateSession
    )


rollResultsDisplay : Maybe Benzino.RoundDetails -> Element.Element Msg
rollResultsDisplay lastEvent =
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
    case lastEvent of
        Nothing ->
            pictogramFor ( Shesh, Yek )

        Just details ->
            pictogramFor details


displayBenzinoScene { account, lastEvent } =
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
                , onPress = Just (PlayerSubmittedBet 1000)
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
            (lastEvent |> rollResultsDisplay)
        , rollTrigger
        ]


view : Model -> Html Msg
view model =
    case model of
        LoadingSession ->
            Element.layout [] Element.none

        CurrentSession aggregates _ ->
            Element.layout [] <|
                Element.column
                    [ Element.width Element.fill
                    , Element.centerX
                    , Element.centerY
                    , Element.spaceEvenly
                    , Element.spacing 48
                    ]
                    [ displayBenzinoScene aggregates

                    -- , plotSession aggregates
                    ]
