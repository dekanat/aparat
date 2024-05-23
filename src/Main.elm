module Main exposing (..)

import Accounting exposing (Account(..))
import Aparat.Shared as Aparat
import Aparat.View
import Browser
import Common.Money exposing (Money)
import Debug exposing (toString)
import Element
import Element.Border
import Element.Input
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
    | BetSubmitted Money
    | PayoutReceived Money


initialSessionWith : Random.Seed -> Session
initialSessionWith seed =
    CurrentSession
        { account = Account 10000
        , innerGame = Aparat.init seed
        }


run : msg -> Cmd msg
run m =
    Task.perform (always m) (Task.succeed ())


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

        ( CurrentSession state, BetSubmitted moneyToBet ) ->
            case state.account |> Accounting.deduct moneyToBet of
                Ok reducedAccount ->
                    let
                        callback =
                            { claimPayout = PayoutReceived }

                        ( nextInnerGameState, outcome ) =
                            state.innerGame
                                |> Aparat.updateWith callback (Aparat.BetPlaced moneyToBet)
                    in
                    ( CurrentSession
                        { state
                            | account = reducedAccount
                            , innerGame = nextInnerGameState
                        }
                    , run outcome
                    )

                Err _ ->
                    ( session
                    , Cmd.none
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
            (innerGame |> Aparat.View.view)
        , rollTrigger
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
