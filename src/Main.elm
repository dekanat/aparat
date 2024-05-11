module Main exposing (..)

import Account exposing (Account(..))
import Aparat exposing (DeterminedEvent, RandomOutcome)
import Benzino exposing (RollOutcome, RoundOutcome(..), RoundState(..), rollingPairOfDice)
import Browser
import Common.Die exposing (Face(..), pictogramFor)
import Common.Money exposing (Money)
import Debug exposing (toString)
import Element
import Element.Border
import Element.Font
import Element.Input
import Html exposing (Html)
import Random



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }



-- MODEL


type alias Model =
    { account : Account
    , round : RoundState
    }


type alias Bet =
    Money


type alias SessionAggregates =
    { history : List DeterminedEvent
    , account : Account
    }


type SessionContext
    = SettledSession SessionAggregates Random.Seed


initContext : () -> ( SessionContext, Cmd Msg )
initContext _ =
    ( SettledSession
        { history = []
        , account = Account 3000
        }
        (Random.initialSeed 1)
    , Cmd.none
    )


type SessionProblem
    = NonRecoverable


playOnce : Money -> SessionContext -> Result SessionProblem SessionContext
playOnce amountToBet session =
    case session of
        SettledSession aggregates seed ->
            let
                resolveBet : Bet -> Account -> SessionContext
                resolveBet betAmount accountAfterBet =
                    let
                        settleWithOutcome : RandomOutcome -> SessionContext
                        settleWithOutcome ( event, nextSeed ) =
                            SettledSession
                                (SessionAggregates
                                    (event :: aggregates.history)
                                    (accountAfterBet |> Account.add event.payout)
                                )
                                nextSeed
                    in
                    seed
                        |> Aparat.determineOutcome betAmount
                        |> settleWithOutcome
            in
            aggregates.account
                |> Account.deduct amountToBet
                |> Result.mapError (\_ -> NonRecoverable)
                |> Result.map (resolveBet amountToBet)



-- seed
--     |> Aparat.determineOutcome bet


type Msg
    = PlayerWantsToBet Money
    | RoundResolves Money RollOutcome


update : Msg -> Model -> ( Model, Cmd Msg )
update msg { account, round } =
    case msg of
        PlayerWantsToBet moneyToBet ->
            case Account.deduct moneyToBet account of
                Ok remainingBalance ->
                    ( { account = remainingBalance
                      , round = Initiated
                      }
                    , Random.generate (RoundResolves moneyToBet) rollingPairOfDice
                    )

                Err _ ->
                    ( { account = account, round = round }, Cmd.none )

        RoundResolves betAmount settledCombination ->
            let
                payout =
                    Aparat.resolvePayout betAmount settledCombination

                newState =
                    { account = Account.add payout account
                    , round = Resolved settledCombination payout
                    }
            in
            ( newState, Cmd.none )


init : () -> ( Model, Cmd Msg )
init _ =
    ( { account = Account 3000
      , round = Resolved ( Yek, Du ) 0
      }
    , Cmd.none
    )



-- VIEW


displayBenzinoScene : { a | account : Account, round : RoundState } -> Element.Element Msg
displayBenzinoScene { account, round } =
    let
        balanceDisplay =
            case account of
                Account b ->
                    Element.text ("Account Balance: " ++ toString b)

        rollResultsDisplay =
            case round of
                Initiated ->
                    Element.text "Rolling..."

                Resolved ( rolledA, rolledB ) _ ->
                    Element.row
                        [ Element.Font.size 200
                        ]
                        [ Element.text (pictogramFor rolledA)
                        , Element.text (pictogramFor rolledB)
                        ]

        rollTrigger =
            Element.Input.button
                [ Element.centerX
                , Element.Border.width 2
                , Element.Border.rounded 2
                , Element.padding 8
                ]
                { label = Element.text "Roll for 1000"
                , onPress = Just (PlayerWantsToBet 1000)
                }
    in
    Element.column []
        [ Element.el
            [ Element.alignRight ]
            balanceDisplay
        , rollResultsDisplay
        , rollTrigger
        ]


view : Model -> Html Msg
view model =
    Element.layout [ Element.padding 50 ]
        (displayBenzinoScene model)
