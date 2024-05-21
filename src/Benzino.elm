module Benzino exposing (..)

import Account exposing (Account(..))
import Aparat exposing (DiceRoll)
import Common.Die exposing (Face(..), glyphFor)
import Common.Money exposing (Money)
import Element
import Element.Font
import Process
import Random
import Task


type alias RoundDetails =
    DiceRoll


type alias RoundOutcome e =
    { event : e
    , payout : Money
    }


type Msg
    = BetPlaced Money
    | DiceRolled RoundDetails


type Request
    = RollDice


type alias Model =
    { seed : Random.Seed
    , event : DiceRoll
    , bet : Money
    }


type Happenings
    = Payout Money


type TalkTheTalk
    = ToSelf Msg
    | ToOthers Happenings


update : Msg -> Model -> ( Model, Cmd TalkTheTalk )
update msg model =
    case msg of
        BetPlaced bet ->
            let
                ( event, seed ) =
                    model.seed |> Random.step Aparat.rollingPairOfDice

                generateOutcome =
                    Process.sleep 1
                        |> Task.andThen (always <| Task.succeed (ToSelf (DiceRolled event)))
                        |> Task.perform identity
            in
            ( { model | bet = bet, seed = seed }, generateOutcome )

        DiceRolled event ->
            let
                money =
                    Aparat.calculatePayout model.bet event

                declarePayout =
                    Process.sleep 1
                        |> Task.andThen (always <| Task.succeed (ToOthers (Payout money)))
                        |> Task.perform identity
            in
            ( { model | event = event }, declarePayout )


view : Model -> Element.Element Msg
view { event } =
    rollResultsDisplay (Just event)


rollResultsDisplay : Maybe RoundDetails -> Element.Element msg
rollResultsDisplay lastEvent =
    let
        xxlSize =
            200

        pictogramFor : ( Face, Face ) -> Element.Element msg
        pictogramFor ( rolledA, rolledB ) =
            Element.row
                [ Element.Font.size xxlSize
                , Element.spacing 8
                ]
                [ Element.text (glyphFor rolledA)
                , Element.text (glyphFor rolledB)
                ]
    in
    case lastEvent of
        Nothing ->
            pictogramFor ( Shesh, Yek )

        Just details ->
            pictogramFor details
