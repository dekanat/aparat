module Superigra.Superigra exposing (..)

import Random
import Random.List
import Superigra.Card exposing (Card)
import Superigra.Deck as Deck


type Request
    = SelectCard Card


type Round
    = Proposed Card (List Card)
    | Resolved Card Card (List Card)
    | UnknownState -- todo: make impossible


update : Request -> Round -> ( Round, Maybe msg )
update request round =
    case round of
        Proposed dealerCard playerChoices ->
            case request of
                SelectCard selectedCard ->
                    let
                        sub =
                            Resolved dealerCard selectedCard playerChoices
                    in
                    ( sub, Nothing )

        _ ->
            ( round, Nothing )


type CardInTheGame
    = FaceUp Card
    | FaceDown Card


dealHand : List Card -> Round
dealHand cardsSelected =
    case cardsSelected of
        dealerCard :: playerChoices ->
            Proposed dealerCard playerChoices

        _ ->
            UnknownState


type alias State =
    Round


init : Random.Seed -> State
init seed =
    let
        dealFromShuffled =
            Deck.freshDeck
                |> Random.List.choices 5
                |> Random.map (Tuple.first >> dealHand)

        ( cards, _ ) =
            seed
                |> Random.step dealFromShuffled
    in
    cards
