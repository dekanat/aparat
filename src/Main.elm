module Main exposing (..)

import Account exposing (Account(..))
import Aparat exposing (DeterminedEvent)
import Benzino exposing (SessionContext(..))
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
        { init = initContext
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }



-- MODEL


type alias Model =
    SessionContext


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


initContext : () -> ( SessionContext, Cmd Msg )
initContext _ =
    ( SettledSession
        { history = []
        , account = Account 2999
        }
        (Random.initialSeed 0)
    , Cmd.none
    )


displayBenzinoScene : { a | account : Account, history : List DeterminedEvent } -> Element.Element Msg
displayBenzinoScene { account, history } =
    let
        balanceDisplay =
            case account of
                Account b ->
                    Element.text ("Account Balance: " ++ toString b)

        rollResultsDisplay =
            case List.head history of
                Nothing ->
                    Element.text "Rolling..."

                Just { roll } ->
                    Element.row
                        [ Element.Font.size 200
                        ]
                        [ Element.text (pictogramFor (Tuple.first roll))
                        , Element.text (pictogramFor (Tuple.second roll))
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
        (case model of
            SettledSession aggregates _ ->
                displayBenzinoScene aggregates
        )
