module Round exposing (..)

import Common.Money exposing (Money)
import Random


type alias GameOfChance e =
    { produceEvent : Random.Generator e
    , derivePayout : Money -> e -> Money
    }


type alias RoundOutcome e =
    { event : e
    , payout : Money
    }


type alias RoundOverview a =
    { seed : Random.Seed
    , details : a
    , bet : Money
    , payout : Money
    }


playRound : GameOfChance e -> Money -> Random.Seed -> ( RoundOutcome e, Random.Seed )
playRound { produceEvent, derivePayout } bet seed =
    let
        ( event, nextSeed ) =
            Random.step produceEvent seed

        payout =
            derivePayout bet event
    in
    ( RoundOutcome event payout, nextSeed )
