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


type alias Model =
    RollResult


init : () -> ( Model, Cmd Msg )
init _ =
    ( ( Panj, Se )
    , Cmd.none
    )



-- UPDATE


type Msg
    = Roll
    | Stop RollResult


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( model
            , Random.generate Stop dieRoller
            )

        Stop rollResults ->
            ( rollResults
            , Cmd.none
            )


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
        [ button [ onClick Roll ] [ text "Spin" ]
        , viewRow model
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
