module History exposing (..)

import Common.Money exposing (Money)
import Random


type alias DeterminedEvent a =
    { details : a
    , seed : Random.Seed
    , bet : Money
    , payout : Money
    }


type alias History e =
    List (DeterminedEvent e)


add : DeterminedEvent e -> History e -> History e
add event history =
    event :: history


last : History e -> Maybe (DeterminedEvent e)
last =
    List.head


empty : History e
empty =
    []
