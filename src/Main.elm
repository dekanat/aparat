module Main exposing (..)

import Accounting.Accounting as Accounting exposing (Account(..))
import Accounting.View
import Aggregate as Aggregate exposing (Aggregate)
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


initialSessionAt : Time.Posix -> Session
initialSessionAt time =
    let
        seed =
            time
                |> Time.posixToMillis
                |> Random.initialSeed
    in
    CurrentSession
        { account = Account 10000
        , innerGame = Aparat.init seed
        }


run : msg -> Cmd msg
run m =
    Task.perform (always m) (Task.succeed ())


accountingLens : Aggregate.Lens SessionState Accounting.Model
accountingLens =
    Aggregate.Lens
        .account
        (\new state -> { state | account = new })


withdrawBetFromAccount : Money -> SessionState -> ( SessionState, Msg )
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


initiateRoundOnAparat : Money -> SessionState -> ( SessionState, Msg )
initiateRoundOnAparat bet =
    let
        updateWithProperCallback =
            Aparat.updateWith
                { claimPayout = PayoutReceived }
    in
    Aparat.InitiateRound bet
        |> Aggregate.performCycle aparatLens updateWithProperCallback


update : Msg -> Model -> ( Model, Cmd Msg )
update msg session =
    case session of
        NoSession ->
            case msg of
                SessionInitiated time ->
                    initialSessionAt time
                        |> withNoCmd

                _ ->
                    NoSession
                        |> withNoCmd

        CurrentSession state ->
            case msg of
                BetOrdered moneyToBet ->
                    state
                        |> withdrawBetFromAccount moneyToBet
                        |> Tuple.mapBoth CurrentSession run

                BetPlaced amount ->
                    state
                        |> initiateRoundOnAparat amount
                        |> Tuple.mapBoth CurrentSession run

                PayoutReceived amount ->
                    let
                        replenishedAccount =
                            state.account
                                |> Accounting.add amount
                    in
                    ( CurrentSession { state | account = replenishedAccount }, Cmd.none )

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
