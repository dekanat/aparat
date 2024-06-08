module Main exposing (..)

import Aggregate
import Aparat.Aparat as Aparat
import Aparat.View
import Browser
import Cmd.Extra exposing (..)
import Common.Money exposing (Money)
import Control.Control as Control
import Control.View
import Dublich.Dublich as Dublich
import Dublich.View
import Element
import Element.Border
import Html exposing (Html)
import Random
import Task



-- MAIN


type alias Flags =
    { currentTime : Int
    }


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


type Msg
    = BetPlaced Money
    | RoundCompleted Money
    | PayoutReceived Money
    | Randomize (Random.Seed -> Msg)
    | SuperGameEvolved Dublich.Request
    | ControlEvolved Control.Request
    | Noop



-- MODEL


type alias Model =
    SessionState


type alias SessionState =
    { innerGame : Aparat.State
    , superGame : Dublich.State
    , control : Control.State
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

        cycleOverControl =
            Aggregate.performCycleOver
                { get = .control
                , set = \new givenState -> { givenState | control = new }
                }

        updateControl =
            Control.updateWith { betPlaced = BetPlaced }
    in
    case msg of
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
                    Control.ReplenishAccount totalPayout
                        |> updateControl
            in
            state |> cycleOverControl collectPayout

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

        ControlEvolved innerMessage ->
            state
                |> cycleOverControl (updateControl innerMessage)

        _ ->
            ( state, Nothing )


init : Flags -> ( Model, Cmd Msg )
init { currentTime } =
    currentTime
        |> Random.initialSeed
        |> arrangeSession 10000
        |> withNoCmd


arrangeSession : Money -> Random.Seed -> SessionState
arrangeSession startedBalance masterSeed =
    { innerGame = Aparat.init masterSeed
    , superGame = Dublich.init 1000
    , control = Control.init startedBalance [ 100, 200, 500, 1000 ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg state =
    case msg of
        Randomize call ->
            state
                |> withCmd
                    (Random.generate call Random.independentSeed)

        _ ->
            state
                |> evolveSessionState msg
                |> Tuple.mapBoth identity runOptional


displayBenzinoScene { innerGame } =
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
            Element.none
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
view state =
    Element.layout [] <|
        Element.column
            [ Element.centerX
            , Element.centerY
            ]
            [ Element.el
                [ Element.centerX
                , Element.padding 48
                ]
                Element.none
            , Element.row
                [ Element.width Element.fill
                , Element.spaceEvenly
                , Element.spacing 48
                ]
                [ displayBenzinoScene state
                , displaySuperGame state
                ]
            , Element.el [ Element.width (Element.px 520) ]
                (Control.View.view ControlEvolved state.control)
            ]
