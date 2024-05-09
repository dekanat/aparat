module Main exposing (..)

import Balance exposing (Balance(..))
import Bet exposing (..)
import Browser
import Core exposing (Money)
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


type GameResult
    = ReturnToPlayer Money


determinePayout : RollOutcome -> Bet -> GameResult
determinePayout ( a, b ) bet =
    case bet of
        Bet amount ->
            let
                winScale =
                    if a == b then
                        6

                    else
                        0
            in
            ReturnToPlayer (amount * winScale)


type RoundState
    = Initiated
    | Resolved RollOutcome GameResult


type alias Model =
    { balance : Balance
    , round : RoundState
    }


type Msg
    = PlayerWantsToBet Money
    | RoundResolves Bet RollOutcome


update : Msg -> Model -> ( Model, Cmd Msg )
update msg { balance, round } =
    case msg of
        PlayerWantsToBet moneyToBet ->
            case makeBet balance moneyToBet of
                Ok ( bet, remainingBalance ) ->
                    ( { balance = remainingBalance
                      , round = Initiated
                      }
                    , Random.generate (RoundResolves bet) rollingPairOfDice
                    )

                Err _ ->
                    ( { balance = balance, round = round }, Cmd.none )

        RoundResolves bet settledCombination ->
            let
                gameResult =
                    bet
                        |> determinePayout settledCombination

                newState =
                    case gameResult of
                        ReturnToPlayer amount ->
                            { balance = amount |> Balance.topUpBalance balance
                            , round = Resolved settledCombination gameResult
                            }
            in
            ( newState, Cmd.none )


init : () -> ( Model, Cmd Msg )
init _ =
    ( { balance = Balance 3000
      , round = Resolved ( Yek, Du ) (ReturnToPlayer 0)
      }
    , Cmd.none
    )



-- VIEW


displayBenzinoScene : { a | balance : Balance, round : RoundState } -> Element.Element Msg
displayBenzinoScene { balance, round } =
    let
        balanceDisplay =
            case balance of
                Balance b ->
                    Element.text ("Balance: " ++ toString b)

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
