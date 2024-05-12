module History exposing (..)


type alias History e =
    List e


add : e -> History e -> History e
add event history =
    event :: history


last : History e -> Maybe e
last =
    List.head


empty : History e
empty =
    []
