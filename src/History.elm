module History exposing (..)

import List.Extra
import Round exposing (RoundOverview)


type alias History e =
    List (RoundOverview e)


add : RoundOverview e -> History e -> History e
add event history =
    history ++ [ event ]


last : History e -> Maybe (RoundOverview e)
last =
    List.Extra.last


empty : History e
empty =
    []
