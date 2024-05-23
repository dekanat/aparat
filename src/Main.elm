module Main exposing (..)

import Accounting.Accounting as Accounting exposing (Account(..))
import Accounting.View
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
    | CurrentSession
        { account : Account
        , innerGame : Aparat.Model
        }


type Msg
    = SessionInitiated Time.Posix
    | Noop
    | BetOrdered Money
    | BetPlaced Money
    | PayoutReceived Money


initialSessionWith : Random.Seed -> Session
initialSessionWith seed =
    CurrentSession
        { account = Account 10000
        , innerGame = Aparat.init seed
        }


initialSessionAt : Time.Posix -> Session
initialSessionAt time =
    time
        |> Time.posixToMillis
        |> Random.initialSeed
        |> initialSessionWith


run : msg -> Cmd msg
run m =
    Task.perform (always m) (Task.succeed ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg session =
    case ( session, msg ) of
        ( NoSession, SessionInitiated time ) ->
            initialSessionAt time
                |> withNoCmd

        ( CurrentSession state, BetOrdered moneyToBet ) ->
            let
                orderBet =
                    Accounting.Withdraw moneyToBet
                        |> Accounting.updateWith
                            { fulfillOrder = BetPlaced
                            , rejectOrder = Noop
                            }

                ( modifiedAccount, callback ) =
                    orderBet state.account

                updatedState =
                    { state | account = modifiedAccount }
            in
            ( CurrentSession updatedState
            , run callback
            )

        ( CurrentSession state, BetPlaced amount ) ->
            let
                resolveRound =
                    Aparat.InitiateRound amount
                        |> Aparat.updateWith
                            { claimPayout = PayoutReceived }

                ( resolution, callback ) =
                    resolveRound state.innerGame

                updatedState =
                    { state | innerGame = resolution }
            in
            ( CurrentSession updatedState
            , run callback
            )

        ( CurrentSession state, PayoutReceived amount ) ->
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
