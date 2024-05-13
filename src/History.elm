module History exposing (..)

import List.Extra
import Round exposing (Round)


type alias History e =
    List (Round e)


add : Round e -> History e -> History e
add event history =
    history ++ [ event ]


last : History e -> Maybe (Round e)
last =
    List.Extra.last


empty : History e
empty =
    []
