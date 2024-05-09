module Bet exposing (..)

import Core exposing (Balance(..), Money)


type Bet
    = Bet Money


type BettingDifficulties
    = NotEnoughAmount


makeBet : Balance -> Money -> Result BettingDifficulties ( Bet, Balance )
makeBet balance amountToBet =
    case balance of
        Balance availableFunds ->
            if amountToBet < availableFunds then
                Ok
                    ( Bet amountToBet
                    , Balance (availableFunds - amountToBet)
                    )

            else
                Err NotEnoughAmount
