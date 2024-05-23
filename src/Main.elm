module Main exposing (..)

import Accounting exposing (Account(..))
import Aparat.Aparat as Aparat
import Aparat.View
import Browser
import Cmd.Extra exposing (..)
import Common.Money exposing (Money)
import ControlPanel.BetControl exposing (betControl)
import Debug exposing (toString)
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
                request =
                    Accounting.Withdraw moneyToBet

                orderBet =
                    Accounting.updateWith
                        { fulfillOrder = BetPlaced
                        , rejectOrder = Noop
                        }

                ( modifiedAccount, callback ) =
                    state.account
                        |> orderBet request
            in
            ( CurrentSession { state | account = modifiedAccount }
            , run callback
            )

        ( CurrentSession state, BetPlaced amount ) ->
            let
                ( resolvedRound, callback ) =
                    state.innerGame
                        |> Aparat.updateWith { claimPayout = PayoutReceived } (Aparat.RoundInitiated amount)
            in
            ( CurrentSession
                { state
                    | innerGame = resolvedRound
                }
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
    let
        balanceDisplay =
            case account of
                Account balance ->
                    Element.text ("Account Balance: " ++ toString balance)
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
            (innerGame |> Aparat.View.view)
        , betControl { orderBet = BetOrdered } ()
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
