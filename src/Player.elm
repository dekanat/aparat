module Player exposing (..)

import Balance exposing (Balance(..), BalanceIssues(..), topUp)
import Common.Money exposing (Money)


makeBet : Money -> Balance -> Result BalanceIssues ( Money, Balance )
makeBet amountToBet balance =
    let
        acceptBet reducedBalance =
            ( amountToBet, reducedBalance )
    in
    Balance.takeFrom balance amountToBet
        |> Result.map acceptBet


roundUp : Money -> Balance -> Balance
roundUp payout balance =
    topUp balance payout
