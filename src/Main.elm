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


evaluateGameResult : Money -> RollResult -> GameResult
evaluateGameResult wager ( a, b ) =
    let
        winScale =
            if a == b then
                6

            else
                0
    in
    MarkWins (wager * winScale)


type RoundState
    = Initiated
    | Resolved RollResult GameResult


type alias Model =
    { balance : Money
    , round : RoundState
    }


type Msg
    = PlayerBets Money
    | RoundResolves Money RollResult


update : Msg -> Model -> ( Model, Cmd Msg )
update msg { balance, round } =
    case msg of
        PlayerBets bet ->
            if bet > balance then
                ( { balance = balance, round = round }, Cmd.none )

            else
                ( { balance = balance - bet
                  , round = Initiated
                  }
                , Random.generate (RoundResolves bet) dieRoller
                )

        RoundResolves bet rollResults ->
            let
                gameResult =
                    evaluateGameResult bet rollResults

                newState =
                    case gameResult of
                        MarkWins amount ->
                            { balance = balance + amount
                            , round = Resolved rollResults gameResult
                            }
            in
            ( newState, Cmd.none )


init : () -> ( Model, Cmd Msg )
init _ =
    ( { balance = 3000, round = Resolved ( Yek, Du ) (MarkWins 0) }
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
