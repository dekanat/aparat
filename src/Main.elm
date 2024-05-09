module Main exposing (..)

import Bet exposing (..)
import Browser
import Debug exposing (toString)
import Die exposing (Face(..), pictogramFor)
import Element
import Element.Border
import Element.Font
import Element.Input
import Html exposing (Html)
import PairOfDice exposing (RollOutcome, rollingPairOfDice)
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


type alias Money =
    Int


type GameResult
    = MarkWins Int


evaluateGameResult : RollOutcome -> Bet -> GameResult
evaluateGameResult ( a, b ) bet =
    case bet of
        Bet amount ->
            let
                winScale =
                    if a == b then
                        6

                    else
                        0
            in
            MarkWins (amount * winScale)


type RoundState
    = Initiated
    | Resolved RollOutcome GameResult


type alias Model =
    { balance : Money
    , round : RoundState
    }


type Msg
    = PlayerBets Money
    | RoundResolves Bet RollOutcome


update : Msg -> Model -> ( Model, Cmd Msg )
update msg { balance, round } =
    case msg of
        PlayerBets moneyToBet ->
            if moneyToBet > balance then
                ( { balance = balance, round = round }, Cmd.none )

            else
                ( { balance = balance - moneyToBet
                  , round = Initiated
                  }
                , Random.generate (RoundResolves (Bet moneyToBet)) rollingPairOfDice
                )

        RoundResolves bet settledCombination ->
            let
                gameResult =
                    bet
                        |> evaluateGameResult settledCombination

                newState =
                    case gameResult of
                        MarkWins amount ->
                            { balance = balance + amount
                            , round = Resolved settledCombination gameResult
                            }
            in
            ( newState, Cmd.none )


init : () -> ( Model, Cmd Msg )
init _ =
    ( { balance = 3000, round = Resolved ( Yek, Du ) (MarkWins 0) }
    , Cmd.none
    )



-- VIEW


displayBenzinoScene : { a | balance : Money, round : RoundState } -> Element.Element Msg
displayBenzinoScene { balance, round } =
    let
        balanceDisplay =
            Element.text ("Balance: " ++ toString balance)

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
                , onPress = Just (PlayerBets 1000)
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
