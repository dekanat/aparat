module History exposing (..)

import Round exposing (Round)


type alias History e =
    List (Round e)


add : Round e -> History e -> History e
add event history =
    history ++ [ event ]


last : History e -> Maybe (Round e)
last =
    List.head


empty : History e
empty =
    []
