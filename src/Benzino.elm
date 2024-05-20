module Benzino exposing (..)

import Account exposing (Account(..))
import Aparat exposing (DiceRoll)
import Common.Die exposing (Face(..), glyphFor)
import Common.Money exposing (Money)
import Element
import Element.Font
import Random


type alias RoundDetails =
    DiceRoll


type alias RoundOutcome e =
    { event : e
    , payout : Money
    }


playRound : Money -> Random.Seed -> ( RoundOutcome DiceRoll, Random.Seed )
playRound bet seed =
    seed
        |> Random.step Aparat.rollingPairOfDice
        |> Tuple.mapFirst
            (\event ->
                { event = event
                , payout = Aparat.calculatePayout bet event
                }
            )


type Msg
    = BetPlaced Money
    | DiceRolled
    | RoundResolved


type alias Model =
    { seed : Random.Seed
    , event : DiceRoll
    , bet : Money
    }


type Effect
    = Payout Money


update : Msg -> Model -> ( Model, List Effect )
update msg model =
    case msg of
        BetPlaced bet ->
            ( { model | bet = bet }, [] )

        DiceRolled ->
            let
                ( event, seed ) =
                    model.seed |> Random.step Aparat.rollingPairOfDice
            in
            ( { model | event = event, seed = seed }, [] )

        RoundResolved ->
            let
                money =
                    Aparat.calculatePayout model.bet model.event
            in
            ( model, [ Payout money ] )


view : Model -> Element.Element Msg
view { event } =
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
    pictogramFor event
