module Player exposing (..)

import Aparat exposing (Payout(..))
import Balance exposing (Balance(..), BalanceIssues(..), topUp)
import Common.Money exposing (Money)


type Bet
    = Bet Money


makeBet : Money -> Balance -> Result BalanceIssues ( Bet, Balance )
makeBet amountToBet balance =
    let
        acceptBet reducedBalance =
            ( Bet amountToBet, reducedBalance )
    in
    Balance.takeFrom balance amountToBet
        |> Result.map acceptBet


roundUp : Payout -> Balance -> Balance
roundUp payout balance =
    case payout of
        Win n ->
            topUp balance n

        Lose _ ->
            balance
