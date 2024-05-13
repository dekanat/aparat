module Core exposing (..)

import Random


type alias SessionContext a =
    ( a, Random.Seed )


type SessionProblem
    = NonRecoverable
