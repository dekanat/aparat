module Round exposing (..)

import Common.Money exposing (Money)
import Random


type alias Round a =
    { seed : Random.Seed
    , details : a
    , bet : Money
    , payout : Money
    }
