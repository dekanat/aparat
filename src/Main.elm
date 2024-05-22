module Main exposing (..)

import Accounting exposing (Account(..))
import Aparat.Benzino
import Aparat.DisplayRound
import Aparat.PairOfDice exposing (PossibleCombination)
import Browser
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
    Session PossibleCombination


type Msg
    = SessionInitiated Time.Posix
    | BetSubmitted Money
    | InnerTalk Aparat.Benzino.TalkTheTalk


initialSessionWith : Random.Seed -> Session e
initialSessionWith seed =
    CurrentSession
        { account = Account 10000
        , innerGame = Aparat.Benzino.init seed
        }


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
                        ( nextInnerGameState, innerCmd ) =
                            state.innerGame |> Aparat.Benzino.update (Aparat.Benzino.BetPlaced moneyToBet)

                        nextState =
                            { state
                                | account = reducedAccount
                                , innerGame =
                                    nextInnerGameState
                            }
                    in
                    ( CurrentSession nextState
                    , innerCmd |> Cmd.map InnerTalk
                    )

                Err _ ->
                    ( session
                    , Cmd.none
                    )

        ( CurrentSession state, InnerTalk innerMsg ) ->
            let
                ( nextState, cmd ) =
                    case innerMsg of
                        Aparat.Benzino.ToSelf igo ->
                            state.innerGame
                                |> Aparat.Benzino.update igo
                                |> Tuple.mapFirst (\ig -> { state | innerGame = ig })
                                |> Tuple.mapSecond (Cmd.map InnerTalk)

                        Aparat.Benzino.ToOthers ogo ->
                            case ogo of
                                Aparat.Benzino.Payout x ->
                                    ( { state | account = state.account |> Accounting.add x }, Cmd.none )
            in
            ( CurrentSession nextState, cmd )

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
            (Just innerGame.event |> Aparat.DisplayRound.view |> Element.map InnerTalk)
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
