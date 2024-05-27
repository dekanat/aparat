module Main exposing (..)

import Accounting.Accounting as Accounting
import Accounting.View
import Aggregate
import Aparat.Aparat as Aparat
import Aparat.View
import Browser
import Cmd.Extra exposing (..)
import Common.Money exposing (Money)
import ControlPanel.View
import Dublich.Dublich as Dublich
import Dublich.View
import Element
import Element.Border
import Html exposing (Html)
import Random
import Task
import Time



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


type Msg
    = SessionInitiated Time.Posix
    | BetOrdered Money
    | BetPlaced Money
    | RoundCompleted Money
    | PayoutReceived Money
    | Randomize (Random.Seed -> Msg)
    | SuperGameEvolved Dublich.Request
    | Noop



-- MODEL


type alias Model =
    Session


type Session
    = NoSession
    | CurrentSession SessionState


type alias SessionState =
    { account : Accounting.State
    , innerGame : Aparat.State
    , superGame : Dublich.State
    }


runOptional : Maybe msg -> Cmd msg
runOptional m =
    case m of
        Just message ->
            Task.perform (always message) (Task.succeed ())

        Nothing ->
            Cmd.none


evolveSessionState : Msg -> SessionState -> ( SessionState, Maybe Msg )
evolveSessionState msg state =
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

        cycleOverSuperGame =
            Aggregate.performCycleOver
                { get = .superGame
                , set = \new givenState -> { givenState | superGame = new }
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
                evaluateRound =
                    Aparat.InitiateRound bet
                        |> Aparat.updateWith
                            { claimPayout = PayoutReceived }
            in
            state |> cycleOverAparat evaluateRound

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

        SuperGameEvolved innerMessage ->
            state
                |> cycleOverSuperGame
                    (innerMessage
                        |> Dublich.updateWith
                            { seed = \inner -> Randomize (SuperGameEvolved << inner)
                            , conclude =
                                \payout ->
                                    Maybe.map PayoutReceived payout
                                        |> Maybe.withDefault Noop
                            }
                    )

        _ ->
            ( state, Nothing )


init : () -> ( Model, Cmd Msg )
init _ =
    ( NoSession
    , Task.perform SessionInitiated Time.now
    )


arrangeSession : Money -> Random.Seed -> Session
arrangeSession startedBalance masterSeed =
    CurrentSession
        { account = Accounting.init startedBalance
        , innerGame = Aparat.init masterSeed
        , superGame = Dublich.init 1000
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg session =
    case session of
        CurrentSession state ->
            case msg of
                Randomize call ->
                    session
                        |> withCmd
                            (Random.generate call Random.independentSeed)

                _ ->
                    state
                        |> evolveSessionState msg
                        |> Tuple.mapBoth CurrentSession runOptional

        NoSession ->
            case msg of
                SessionInitiated time ->
                    time
                        |> (Random.initialSeed << Time.posixToMillis)
                        |> arrangeSession 10000
                        |> withNoCmd

                _ ->
                    NoSession
                        |> withNoCmd


displayBenzinoScene { account, innerGame } =
    Element.column
        [ Element.width (Element.px 520)
        , Element.centerX
        , Element.Border.width 2
        , Element.Border.rounded 2
        , Element.padding 32
        , Element.spacing 8
        ]
        [ Element.el [ Element.centerX ]
            (Aparat.View.gameScene innerGame)
        , Element.el [ Element.centerX ]
            (ControlPanel.View.betControl { orderBet = BetOrdered } ())
        ]


displaySuperGame { superGame } =
    Element.column
        [ Element.width (Element.px 520)
        , Element.centerX
        , Element.Border.width 2
        , Element.Border.rounded 2
        , Element.padding 32
        , Element.spacing 8
        ]
        [ Dublich.View.view
            { toSelf = SuperGameEvolved
            }
            superGame
        ]


view : Model -> Html Msg
view model =
    case model of
        NoSession ->
            Element.layout [] Element.none

        CurrentSession state ->
            Element.layout [] <|
                Element.column
                    [ Element.centerX
                    , Element.centerY
                    ]
                    [ Element.el [ Element.alignRight ] (Accounting.View.balanceDisplay state.account)
                    , Element.row
                        [ Element.width Element.fill
                        , Element.spaceEvenly
                        , Element.spacing 48
                        ]
                        [ displayBenzinoScene state
                        , displaySuperGame state
                        ]
                    ]
