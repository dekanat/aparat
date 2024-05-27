module Dublich.Dublich exposing (..)

import Dublich.Card exposing (Card(..), regularCards)
import Dublich.Deck as Deck
import Random


type Request
    = DealCards Random.Seed
    | SelectCard Card


type State
    = Initial (List Card)
    | Proposed Card (List Card)
    | Resolved Card Card (List Card)


update : Request -> State -> ( State, Maybe msg )
update request round =
    case ( round, request ) of
        ( Initial _, DealCards seed ) ->
            let
                ( dealerCard, playerChoices ) =
                    seed
                        |> Random.step (Deck.dealHandFromTop 4)
                        |> Tuple.first
            in
            ( Proposed dealerCard playerChoices, Nothing )

        ( Proposed dealerCard playerChoices, SelectCard selectedCard ) ->
            let
                sub =
                    Resolved dealerCard selectedCard playerChoices
            in
            ( sub, Nothing )

        _ ->
            ( round, Nothing )


init : Random.Seed -> State
init _ =
    Initial regularCards
