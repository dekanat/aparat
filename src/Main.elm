module Main exposing (..)

import Account exposing (Account(..))
import Benzino
import Browser
import Common.Die exposing (Face(..))
import Common.Money exposing (Money)
import Debug exposing (toString)
import Element
import Element.Border
import Element.Input
import Html exposing (Html)
import Random
import Session exposing (Session(..), SessionProblem(..))
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
    Session Benzino.RoundDetails


type Msg
    = SessionInitiated Time.Posix
    | BetSubmitted Money


initialSessionWith : Random.Seed -> Session e
initialSessionWith seed =
    CurrentSession
        { lastEvent = Nothing
        , account = Account 10000
        , innerGame =
            { seed = seed
            , bet = 0
            , event = ( Yek, Yek )
            }
        }
        seed


update : Msg -> Model -> ( Model, Cmd Msg )
update msg session =
    case ( session, msg ) of
        ( NoSession, SessionInitiated time ) ->
            let
                firstSeed =
                    time
                        |> Time.posixToMillis
                        |> Random.initialSeed
            in
            ( initialSessionWith firstSeed
            , Cmd.none
            )

        ( CurrentSession state seed, BetSubmitted moneyToBet ) ->
            case state.account |> Account.deduct moneyToBet of
                Ok reducedAccount ->
                    let
                        evolveState { payout, event } =
                            { state
                                | account = reducedAccount |> Account.add payout
                                , lastEvent = Just event
                            }

                        ( settledState, nextSeed ) =
                            Benzino.playRound moneyToBet seed
                                |> Tuple.mapFirst evolveState
                    in
                    ( CurrentSession settledState nextSeed
                    , Cmd.none
                    )

                Err _ ->
                    ( session
                    , Cmd.none
                    )

        _ ->
            ( NoSession, Cmd.none )


initiateSession : Cmd Msg
initiateSession =
    Task.perform SessionInitiated Time.now



-- VIEW


init : () -> ( Model, Cmd Msg )
init _ =
    ( NoSession
    , initiateSession
    )


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
                , onPress = Just (BetSubmitted 1000)
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
            (lastEvent |> Benzino.rollResultsDisplay)
        , rollTrigger
        ]


view : Model -> Html Msg
view model =
    case model of
        NoSession ->
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
                    ]
