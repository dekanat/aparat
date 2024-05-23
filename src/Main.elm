module Main exposing (..)

import Accounting.Accounting as Accounting exposing (Account(..))
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
import List.Extra exposing (cycle)
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
    { account : Account
    , innerGame : Aparat.State
    }


type Msg
    = SessionInitiated Time.Posix
    | Noop
    | BetOrdered Money
    | BetPlaced Money
    | PayoutReceived Money


run : Maybe msg -> Cmd msg
run m =
    case m of
        Just message ->
            Task.perform (always message) (Task.succeed ())

        Nothing ->
            Cmd.none


lensForAccounting : Aggregate.Lens SessionState Accounting.Model
lensForAccounting =
    { get = .account
    , set = \new state -> { state | account = new }
    }


withdrawBetFromAccount bet =
    Accounting.updateWith
        { fulfillOrder = BetPlaced
        , rejectOrder = Noop
        }
        (Accounting.Withdraw bet)


replenishAccount : Money -> Aggregate.UpdateInner Accounting.Model Msg
replenishAccount payout =
    Accounting.updateWith
        { fulfillOrder = \_ -> Noop
        , rejectOrder = Noop
        }
        (Accounting.Replenish payout)


lensForAparat : Aggregate.Lens SessionState Aparat.State
lensForAparat =
    { get = .innerGame
    , set = \new state -> { state | innerGame = new }
    }


initiateRoundOnAparat bet =
    Aparat.updateWith
        { claimPayout = PayoutReceived }
        (Aparat.InitiateRound bet)


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
            cycleOverAccounting (withdrawBetFromAccount amountToBet) state

        BetPlaced bet ->
            cycleOverAparat (initiateRoundOnAparat bet) state

        PayoutReceived totalPayout ->
            cycleOverAccounting (replenishAccount totalPayout) state

        _ ->
            ( state, Nothing )


init : () -> ( Model, Cmd Msg )
init _ =
    ( NoSession
    , Task.perform SessionInitiated Time.now
    )


initSessionAt : Time.Posix -> ( Model, Cmd Msg )
initSessionAt time =
    let
        seed =
            time
                |> Time.posixToMillis
                |> Random.initialSeed

        initialState =
            { account = Account 10000
            , innerGame = Aparat.init seed
            }
    in
    ( CurrentSession initialState, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg session =
    case session of
        NoSession ->
            case msg of
                SessionInitiated time ->
                    initSessionAt time

                _ ->
                    NoSession
                        |> withNoCmd

        CurrentSession state ->
            state
                |> evolve msg
                |> Tuple.mapBoth CurrentSession run


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
