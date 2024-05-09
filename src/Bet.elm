module Bet exposing (..)

import Core exposing (Money)


type Bet
    = Bet Money


type BettingDifficulties
    = NotEnoughAmount


makeBet : Money -> Money -> Result BettingDifficulties ( Money, Bet )
makeBet balance amount =
    if amount < balance then
        Ok ( balance - amount, Bet amount )

    else
        Err NotEnoughAmount
