-- Press a button to draw a random card.
--
-- Dependencies:
--   elm install elm/random
--


module Main exposing (Board, Card(..), Model, Msg(..), Row, cardGenerator, init, main, manyCardGenerator, subscriptions, update, view, viewCard, viewRow)

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
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Row =
    List Card


type alias Board =
    List Row


type alias Model =
    List Card


init : () -> ( Model, Cmd Msg )
init _ =
    ( [ Queen, Queen, Queen ]
    , Cmd.none
    )


type Card
    = Ace
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King



-- UPDATE


type Msg
    = Spin
    | Stop (List Card)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Spin ->
            ( model
            , Random.generate Stop manyCardGenerator
            )

        Stop cards ->
            ( cards
            , Cmd.none
            )


manyCardGenerator : Random.Generator (List Card)
manyCardGenerator =
    Random.list 3 cardGenerator


cardGenerator : Random.Generator Card
cardGenerator =
    Random.uniform Ace
        [ Two
        , Three
        , Four
        , Five
        , Six
        , Seven
        , Eight
        , Nine
        , Ten
        , Jack
        , Queen
        , King
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Spin ] [ text "Spin" ]
        , viewRow model
        ]


viewRow : List Card -> Html Msg
viewRow cards =
    let
        viewEach : Card -> Html Msg
        viewEach card =
            span [ style "font-size" "12em" ] [ text (viewCard card) ]
    in
    div []
        (List.map viewEach cards)


viewCard : Card -> String
viewCard card =
    case card of
        Ace ->
            "🂡"

        Two ->
            "🂢"

        Three ->
            "🂣"

        Four ->
            "🂤"

        Five ->
            "🂥"

        Six ->
            "🂦"

        Seven ->
            "🂧"

        Eight ->
            "🂨"

        Nine ->
            "🂩"

        Ten ->
            "🂪"

        Jack ->
            "🂫"

        Queen ->
            "🂭"

        King ->
            "🂮"
