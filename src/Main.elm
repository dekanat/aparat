module Main exposing (..)

import Browser
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


type alias Money =
    Int


type DieFace
    = Yek
    | Du
    | Se
    | Jhar
    | Panj
    | Shesh


type alias RollResult =
    ( DieFace, DieFace )


type GameResult
    = MarkWins Int
    | ZaraWins Int


evaluateGameResult : Money -> RollResult -> GameResult
evaluateGameResult wager ( a, b ) =
    if a == b then
        MarkWins (wager * 6)

    else
        ZaraWins wager


type alias Accounts =
    { mark : Money, zara : Money }


type GameState
    = Staked Money
    | Resolved RollResult GameResult


type alias Model =
    { balance : Money
    , gameState : GameState
    }


type Msg
    = Bet Money
    | GameResolves RollResult


update : Msg -> Model -> ( Model, Cmd Msg )
update msg { balance, gameState } =
    case msg of
        Bet amount ->
            let
                newModel =
                    { balance = balance - amount
                    , gameState = Staked amount
                    }
            in
            ( newModel, Random.generate GameResolves dieRoller )

        GameResolves rollResults ->
            let
                wager =
                    case gameState of
                        Staked bet ->
                            bet

                        _ ->
                            0

                gameResult =
                    evaluateGameResult wager rollResults

                newState =
                    case gameResult of
                        MarkWins amount ->
                            { balance = balance + amount
                            , gameState = Resolved rollResults gameResult
                            }

                        ZaraWins _ ->
                            { balance = balance
                            , gameState = Resolved rollResults gameResult
                            }
            in
            ( newState, Cmd.none )


init : () -> ( Model, Cmd Msg )
init _ =
    ( { balance = 3000, gameState = Resolved ( Yek, Du ) (MarkWins 0) }
    , Cmd.none
    )



-- UPDAT


dieRoller : Random.Generator RollResult
dieRoller =
    Random.pair dieGenerator dieGenerator


dieGenerator : Random.Generator DieFace
dieGenerator =
    Random.uniform Yek
        [ Du
        , Se
        , Jhar
        , Panj
        , Shesh
        ]



-- VIEW


displayBenzinoScene : { a | balance : Money, gameState : GameState } -> Element.Element Msg
displayBenzinoScene { balance, gameState } =
    let
        balanceDisplay =
            Element.text ("Balance: " ++ toString balance)

        rollResultsDisplay =
            case gameState of
                Staked _ ->
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
                , onPress = Just (Bet 1000)
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


pictogramFor : DieFace -> String
pictogramFor dieFace =
    case dieFace of
        Yek ->
            "⚀"

        Du ->
            "⚁"

        Se ->
            "⚂"

        Jhar ->
            "⚃"

        Panj ->
            "⚄"

        Shesh ->
            "⚅"
