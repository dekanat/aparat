module Balance exposing (..)

import Common.Money exposing (Money)


type Balance
    = Balance Money


topUp : Balance -> Money -> Balance
topUp currentBalance amountToAdd =
    case currentBalance of
        Balance availableAmount ->
            Balance (availableAmount + amountToAdd)


type BalanceIssues
    = InsufficientBalance


takeFrom : Balance -> Money -> Result BalanceIssues Balance
takeFrom initialBalance amountToTake =
    case initialBalance of
        Balance initialAmount ->
            if initialAmount > amountToTake then
                Ok (Balance (initialAmount - amountToTake))

            else
                Err InsufficientBalance
