module Dublich.Dublich exposing (..)

import Common.Money exposing (Money)
import Dublich.Card as Card exposing (Card(..))
import Dublich.Deck as Deck
import Random


type Request
    = DealCards Random.Seed
    | SelectCard Card
    | Start


type alias Choice =
    Card


type alias Box =
    ( Card, List Choice )


type State
    = Initial Money
    | Proposed Money Box
    | Resolved (Maybe Money) Box Card


type alias CallbackInterface msg =
    { seed : (Random.Seed -> Request) -> msg
    , conclude : Maybe Money -> msg
    }


updateWith : CallbackInterface msg -> Request -> State -> ( State, Maybe msg )
updateWith { seed, conclude } request state =
    case ( state, request ) of
        ( Initial stake, DealCards variance ) ->
            let
                ( dealerCard, playerChoices ) =
                    variance
                        |> Random.step (Deck.dealHandFromTop 4)
                        |> Tuple.first
            in
            ( Proposed stake ( dealerCard, playerChoices ), Nothing )

        ( Initial stake, Start ) ->
            ( state, Just (seed DealCards) )

        ( Proposed stake box, SelectCard selectedCard ) ->
            let
                sub =
                    Resolved (Just stake) box selectedCard

                outcome =
                    case Card.compare selectedCard (Tuple.first box) of
                        LT ->
                            conclude Nothing

                        EQ ->
                            conclude (Just (stake + 0))

                        GT ->
                            conclude (Just (stake + stake))
            in
            ( sub, Just outcome )

        _ ->
            ( state, Nothing )


init : Money -> State
init stake =
    Initial stake
