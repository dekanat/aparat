module Benzino exposing (..)

import Accounting exposing (Account(..))
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


type alias DiceRoll =
    ( Face, Face )


rollingPairOfDice : Random.Generator DiceRoll
rollingPairOfDice =
    Random.pair rollingDie rollingDie


calculatePayout : Money -> DiceRoll -> Money
calculatePayout betAmount ( rolledA, rolledB ) =
    if rolledA == rolledB then
        betAmount * 6

    else
        0


type Face
    = Yek
    | Du
    | Se
    | Jhar
    | Panj
    | Shesh


rollingDie : Random.Generator Face
rollingDie =
    Random.uniform Yek
        [ Du
        , Se
        , Jhar
        , Panj
        , Shesh
        ]


glyphFor : Face -> String
glyphFor face =
    case face of
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


init : Random.Seed -> Model
init seed =
    { seed = seed
    , bet = 0
    , event = ( Yek, Yek )
    }


update : Msg -> Model -> ( Model, Cmd TalkTheTalk )
update msg model =
    case msg of
        BetPlaced bet ->
            let
                ( event, seed ) =
                    model.seed |> Random.step rollingPairOfDice

                generateOutcome =
                    Process.sleep 1
                        |> Task.andThen (always <| Task.succeed (ToSelf (DiceRolled event)))
                        |> Task.perform identity
            in
            ( { model | bet = bet, seed = seed }, generateOutcome )

        DiceRolled event ->
            let
                money =
                    calculatePayout model.bet event

                declarePayout =
                    Process.sleep 1
                        |> Task.andThen (always <| Task.succeed (ToOthers (Payout money)))
                        |> Task.perform identity
            in
            ( { model | event = event }, declarePayout )


view : Model -> Element.Element TalkTheTalk
view { event } =
    rollResultsDisplay (Just event)


rollResultsDisplay : Maybe RoundDetails -> Element.Element TalkTheTalk
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
