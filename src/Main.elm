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
    , innerGame : Aparat.Model
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


accountingLens : Aggregate.Lens SessionState Accounting.Model
accountingLens =
    Aggregate.Lens
        .account
        (\new state -> { state | account = new })


withdrawBetFromAccount : Money -> SessionState -> ( SessionState, Maybe Msg )
withdrawBetFromAccount bet =
    let
        updateWithProperCallback =
            Accounting.updateWith
                { fulfillOrder = BetPlaced
                , rejectOrder = Noop
                }
    in
    Accounting.Withdraw bet
        |> Aggregate.performCycle accountingLens updateWithProperCallback


aparatLens : Aggregate.Lens SessionState Aparat.Model
aparatLens =
    { get = .innerGame
    , set = \new state -> { state | innerGame = new }
    }


initiateRoundOnAparat : Money -> SessionState -> ( SessionState, Maybe Msg )
initiateRoundOnAparat bet =
    let
        updateWithProperCallback =
            Aparat.updateWith
                { claimPayout = PayoutReceived }
    in
    Aparat.InitiateRound bet
        |> Aggregate.performCycle aparatLens updateWithProperCallback


evolve : Msg -> SessionState -> ( SessionState, Maybe Msg )
evolve msg state =
    case msg of
        BetOrdered amount ->
            state
                |> withdrawBetFromAccount amount

        BetPlaced amount ->
            state
                |> initiateRoundOnAparat amount

        PayoutReceived amount ->
            let
                replenishedAccount =
                    state.account
                        |> Accounting.add amount
            in
            ( { state | account = replenishedAccount }, Nothing )

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
