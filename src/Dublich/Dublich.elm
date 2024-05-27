module Dublich.Dublich exposing (..)

import Common.Money exposing (Money)
import Dublich.Card exposing (Card(..))
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
            in
            ( sub, Nothing )

        _ ->
            ( state, Nothing )



-- update : Request -> State -> ( State, Maybe msg )
-- update request round =
--     case ( round, request ) of
--         ( Initial stake, DealCards seed ) ->
--             let
--                 ( dealerCard, playerChoices ) =
--                     seed
--                         |> Random.step (Deck.dealHandFromTop 4)
--                         |> Tuple.first
--             in
--             ( Proposed stake ( dealerCard, playerChoices ), Nothing )
--         ( Proposed stake box, SelectCard selectedCard ) ->
--             let
--                 sub =
--                     Resolved (Just stake) box selectedCard
--             in
--             ( sub, Nothing )
--         _ ->
--             ( round, Nothing )


init : Random.Seed -> State
init _ =
    Initial 1000
