module Main exposing (..)

import Account exposing (Account(..))
import Benzino
import Browser
import Chart as C
import Chart.Attributes as CA
import Common.Die exposing (Face(..), glyphFor)
import Common.Money exposing (Money)
import Debug exposing (toString)
import Element
import Element.Border
import Element.Font
import Element.Input
import History exposing (History)
import Html exposing (Html)
import List.Extra
import Random
import Session exposing (Session, SessionState)
import Task exposing (..)
import Time exposing (..)
import SessionPlot exposing (plotSession)



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
    { session : Session Benzino.RoundDetails
    , initialRandomSeed : Time.Posix
    }


type Msg
    = PlayerWantsToBet Money
    | FreshSeed Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg session =
    case msg of
        PlayerWantsToBet moneyToBet ->
            case Benzino.playOnce moneyToBet session.session of
                Ok ctx ->
                    ( { session = ctx, initialRandomSeed = session.initialRandomSeed }
                    , Cmd.none
                    )

                Err _ ->
                    ( session, Cmd.none )

        FreshSeed time ->
            ( { session =
                    ( Tuple.first session.session
                    , Random.initialSeed (Time.posixToMillis time)
                    )
              , initialRandomSeed = time
              }
            , Cmd.none
            )


getFreshSeed : Cmd Msg
getFreshSeed =
    Task.perform FreshSeed Time.now



-- VIEW


init : () -> ( Model, Cmd Msg )
init _ =
    ( { session =
            ( { history = History.empty
              , account = Account 10000
              }
            , Random.initialSeed 0
            )
      , initialRandomSeed = Time.millisToPosix 0
      }
    , getFreshSeed
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
                , Element.spacing 8
                ]
                [ Element.text (glyphFor rolledA)
                , Element.text (glyphFor rolledB)
                ]
    in
    case History.last history of
        Nothing ->
            pictogramFor ( Shesh, Yek )

        Just { details } ->
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
    Element.column
        [ Element.width (Element.px 520)
        , Element.centerX
        , Element.Border.width 2
        , Element.Border.rounded 2
        , Element.padding 32
        , Element.spacing 8
        ]
        [ Element.el
            [ Element.alignRight ]
            balanceDisplay
        , Element.el
            [ Element.centerX ]
            (rollResultsDisplay history)
        , rollTrigger
        ]


view : Model -> Html Msg
view model =
    case model.session of
        ( aggregates, _ ) ->
            Element.layout [] <|
                Element.column
                    [ Element.width Element.fill
                    , Element.centerX
                    , Element.centerY
                    , Element.spaceEvenly
                    , Element.spacing 48
                    ]
                    [ displayBenzinoScene aggregates
                    , plotSession aggregates
                    ]

