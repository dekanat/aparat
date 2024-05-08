module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
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


stake : Money -> RollResult -> GameResult
stake wager ( a, b ) =
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
                isWinningCombination =
                    case rollResults of
                        ( Yek, Yek ) ->
                            True

                        ( Du, Du ) ->
                            True

                        ( Se, Se ) ->
                            True

                        ( Jhar, Jhar ) ->
                            True

                        ( Panj, Panj ) ->
                            True

                        ( Shesh, Shesh ) ->
                            True

                        _ ->
                            False

                wager =
                    case gameState of
                        Staked amount ->
                            amount

                        _ ->
                            0

                gameResult =
                    if isWinningCombination then
                        MarkWins (wager * 6)

                    else
                        ZaraWins wager

                newModel =
                    case gameResult of
                        MarkWins amount ->
                            { balance = balance + amount, gameState = Resolved rollResults gameResult }

                        ZaraWins amount ->
                            { balance = balance - amount, gameState = Resolved rollResults gameResult }
            in
            ( newModel, Cmd.none )


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


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick (Bet 1000) ] [ text "Spin" ]
        , case model.gameState of
            Staked _ ->
                text "Spinning..."

            Resolved rollResult _ ->
                div []
                    [ viewRow rollResult
                    ]
        , div []
            [ text ("Mark: " ++ String.fromInt model.balance)
            ]
        ]


viewRow : RollResult -> Html Msg
viewRow rollResult =
    let
        viewEach : DieFace -> Html Msg
        viewEach dieFace =
            span [ style "font-size" "12em" ] [ text (viewDieFace dieFace) ]

        ( l, r ) =
            rollResult
    in
    div []
        (List.map viewEach [ l, r ])


viewDieFace : DieFace -> String
viewDieFace dieFace =
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
