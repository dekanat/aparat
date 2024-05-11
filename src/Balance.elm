module Balance exposing (..)

import Common.Money exposing (Money)


type Balance
    = Balance Money


topUp2 : Balance -> Money -> Balance
topUp2 currentBalance amountToAdd =
    case currentBalance of
        Balance availableAmount ->
            Balance (availableAmount + amountToAdd)


topUp a b =
    topUp2 b a


toMoney : Balance -> Money
toMoney balance =
    case balance of
        Balance amount ->
            amount


type BalanceIssues
    = InsufficientBalance


takeFrom : Balance -> Money -> Result BalanceIssues Balance
takeFrom initialBalance amountToTake =
    case initialBalance of
        Balance initialAmount ->
            if initialAmount >= amountToTake then
                Ok (Balance (initialAmount - amountToTake))

            else
                Err InsufficientBalance


take a b =
    takeFrom b a
