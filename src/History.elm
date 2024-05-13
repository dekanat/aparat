module History exposing (..)

import Account exposing (Account)
import Round exposing (Round)


type alias RoundSnapshot e =
    ( Account, Round e )


type alias History e =
    List (RoundSnapshot e)


record : RoundSnapshot e -> History e -> History e
record ( accountDuringRound, round ) history =
    history ++ [ ( accountDuringRound, round ) ]


last : History e -> Maybe (RoundSnapshot e)
last =
    List.head


empty : History e
empty =
    []
