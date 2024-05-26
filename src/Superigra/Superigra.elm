module Superigra.Superigra exposing (..)

import Random
import Random.List
import Superigra.Card as Card exposing (Card)
import Superigra.Deck as Deck exposing (Deck, freshDeck)


type Request
    = DealCards Random.Seed
    | SelectCard Card


type Round
    = Initial Deck
    | Proposed Card (List Card)
    | Resolved Card Card (List Card)
    | UnknownState -- todo: make impossible


update : Request -> Round -> ( Round, Maybe msg )
update request round =
    case ( round, request ) of
        ( Initial _, DealCards seed ) ->
            let
                dealFromShuffled =
                    Deck.freshDeck
                        |> Random.List.choices 5
                        |> Random.map (Tuple.first >> dealHand)

                ( cards, _ ) =
                    seed
                        |> Random.step dealFromShuffled
            in
            ( cards, Nothing )

        ( Proposed dealerCard playerChoices, SelectCard selectedCard ) ->
            let
                sub =
                    Resolved dealerCard selectedCard playerChoices
            in
            ( sub, Nothing )

        _ ->
            ( round, Nothing )


dealShuffled : Int -> Card -> List Card -> Random.Generator ( Card, List Card )
dealShuffled count first rest =
    let
        withChoicesWithoutReplacement : Card -> Random.Generator ( Card, List Card )
        withChoicesWithoutReplacement dealerCard =
            rest
                |> List.filter ((/=) dealerCard)
                |> Random.List.choices count
                |> Random.map (\( playerChoices, _ ) -> ( dealerCard, playerChoices ))
    in
    Random.uniform first rest
        |> Random.andThen withChoicesWithoutReplacement


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
    Initial freshDeck
