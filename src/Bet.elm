module Bet exposing (..)


type Bet
    = Bet Int


type BettingDifficulties
    = NotEnoughAmount


makeBet : Int -> Int -> Result BettingDifficulties ( Int, Bet )
makeBet balance amount =
    if amount < balance then
        Ok ( balance - amount, Bet amount )

    else
        Err NotEnoughAmount
