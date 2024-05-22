module Aparat.Benzino exposing (..)

import Accounting exposing (Account(..))
import Aparat.DisplayRound
import Aparat.Model exposing (DieFace(..), Model, PossibleCombination)
import Aparat.PairOfDice exposing (fairPairOfDice)
import Common.Money exposing (Money)
import Element
import Process
import Random
import Task


type alias RoundOutcome e =
    { event : e
    , payout : Money
    }


type Msg
    = BetPlaced Money
    | DiceRolled PossibleCombination


type Request
    = RollDice


type Happenings
    = Payout Money


type TalkTheTalk
    = ToSelf Msg
    | ToOthers Happenings


calculatePayout : Money -> PossibleCombination -> Money
calculatePayout betAmount ( rolledA, rolledB ) =
    if rolledA == rolledB then
        betAmount * 6

    else
        0


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
                    model.seed |> Random.step fairPairOfDice

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
    Aparat.DisplayRound.view (Just event)
