module Superigra.Superigra exposing (..)

import Random
import Superigra.Card exposing (Card(..), regularCards)
import Superigra.Deck as Deck


type Request
    = DealCards Random.Seed
    | SelectCard Card


type State
    = Initial (List Card)
    | Proposed Card (List Card)
    | Resolved Card Card (List Card)
    | UnknownState -- todo: make impossible


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
