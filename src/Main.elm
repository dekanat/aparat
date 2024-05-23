module Main exposing (..)

import Accounting.Accounting as Accounting
import Accounting.View
import Aggregate as Aggregate
import Aparat.Aparat as Aparat
import Aparat.View
import Browser
import Cmd.Extra exposing (..)
import Common.Money exposing (Money)
import ControlPanel.View
import Element
import Element.Border
import Html exposing (Html)
import Random
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
    Session


type Session
    = NoSession
    | CurrentSession SessionState


type alias SessionState =
    { account : Accounting.State
    , innerGame : Aparat.State
    }


type Msg
    = SessionInitiated Time.Posix
    | Noop
    | BetOrdered Money
    | BetPlaced Money
    | PayoutReceived Money


runOptional : Maybe msg -> Cmd msg
runOptional m =
    case m of
        Just message ->
            Task.perform (always message) (Task.succeed ())

        Nothing ->
            Cmd.none


evolve : Msg -> SessionState -> ( SessionState, Maybe Msg )
evolve msg state =
    let
        cycleOverAccounting =
            Aggregate.performCycleOver
                { get = .account
                , set = \new givenState -> { givenState | account = new }
                }

        cycleOverAparat =
            Aggregate.performCycleOver
                { get = .innerGame
                , set = \new givenState -> { givenState | innerGame = new }
                }
    in
    case msg of
        BetOrdered amountToBet ->
            let
                withdrawBet =
                    Accounting.Withdraw amountToBet
                        |> Accounting.updateWith
                            { fulfillOrder = BetPlaced
                            , rejectOrder = Noop
                            }
            in
            state |> cycleOverAccounting withdrawBet

        BetPlaced bet ->
            let
                initiateRound =
                    Aparat.InitiateRound bet
                        |> Aparat.updateWith
                            { claimPayout = PayoutReceived }
            in
            state |> cycleOverAparat initiateRound

        PayoutReceived totalPayout ->
            let
                collectPayout =
                    Accounting.Replenish totalPayout
                        |> Accounting.updateWith
                            { fulfillOrder = \_ -> Noop
                            , rejectOrder = Noop
                            }
            in
            state |> cycleOverAccounting collectPayout

        _ ->
            ( state, Nothing )


init : () -> ( Model, Cmd Msg )
init _ =
    ( NoSession
    , Task.perform SessionInitiated Time.now
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg session =
    case session of
        NoSession ->
            case msg of
                SessionInitiated time ->
                    let
                        seed =
                            time
                                |> Time.posixToMillis
                                |> Random.initialSeed

                        initialState =
                            { account = Accounting.init 10000
                            , innerGame = Aparat.init seed
                            }
                    in
                    CurrentSession initialState
                        |> withNoCmd

                _ ->
                    NoSession
                        |> withNoCmd

        CurrentSession state ->
            state
                |> evolve msg
                |> Tuple.mapBoth CurrentSession runOptional


displayBenzinoScene { account, innerGame } =
    Element.column
        [ Element.width (Element.px 520)
        , Element.centerX
        , Element.Border.width 2
        , Element.Border.rounded 2
        , Element.padding 32
        , Element.spacing 8
        ]
        [ Element.el [ Element.alignRight ]
            (Accounting.View.balanceDisplay account)
        , Element.el [ Element.centerX ]
            (Aparat.View.gameScene innerGame)
        , Element.el [ Element.centerX ]
            (ControlPanel.View.betControl { orderBet = BetOrdered } ())
        ]


view : Model -> Html Msg
view model =
    case model of
        NoSession ->
            Element.layout [] Element.none

        CurrentSession state ->
            Element.layout [] <|
                Element.column
                    [ Element.width Element.fill
                    , Element.centerX
                    , Element.centerY
                    , Element.spaceEvenly
                    , Element.spacing 48
                    ]
                    [ displayBenzinoScene state
                    ]
