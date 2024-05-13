module Main exposing (..)

import Account exposing (Account(..))
import Benzino
import Browser
import Common.Die exposing (Face(..), glyphFor)
import Common.Money exposing (Money)
import Debug exposing (toString)
import Element
import Element.Border
import Element.Font
import Element.Input
import History exposing (History)
import Html exposing (Html)
import Random
import Session exposing (Session, SessionState)



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
    Session Benzino.RoundDetails


type Msg
    = PlayerWantsToBet Money


update : Msg -> Model -> ( Model, Cmd Msg )
update msg session =
    case msg of
        PlayerWantsToBet moneyToBet ->
            case Benzino.playOnce moneyToBet session of
                Ok ctx ->
                    ( ctx
                    , Cmd.none
                    )

                Err _ ->
                    ( session, Cmd.none )



-- VIEW


init : () -> ( Model, Cmd Msg )
init _ =
    ( ( { history = History.empty
        , account = Account 3000
        }
      , Random.initialSeed 0
      )
    , Cmd.none
    )


rollResultsDisplay : History Benzino.RoundDetails -> Element.Element Msg
rollResultsDisplay history =
    let
        xxlSize =
            200

        pictogramFor : ( Face, Face ) -> Element.Element Msg
        pictogramFor ( rolledA, rolledB ) =
            Element.row
                [ Element.Font.size xxlSize
                ]
                [ Element.text (glyphFor rolledA)
                , Element.text (glyphFor rolledB)
                ]
    in
    case History.last history of
        Nothing ->
            pictogramFor ( Shesh, Yek )

        Just ( _, { details } ) ->
            pictogramFor details


displayBenzinoScene : SessionState Benzino.RoundDetails -> Element.Element Msg
displayBenzinoScene { account, history } =
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
                , onPress = Just (PlayerWantsToBet 1000)
                }
    in
    Element.column []
        [ Element.el
            [ Element.alignRight ]
            balanceDisplay
        , rollResultsDisplay history
        , rollTrigger
        ]


view : Model -> Html Msg
view ( aggregates, _ ) =
    Element.layout [ Element.padding 50 ]
        (displayBenzinoScene aggregates)
