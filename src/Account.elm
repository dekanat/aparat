module Account exposing (..)

import Common.Money exposing (Money)


type Account
    = Account Money


add : Money -> Account -> Account
add amount account =
    case account of
        Account current ->
            Account (current + amount)


type AccountingProblem
    = InsufficientBalance


deduct : Money -> Account -> Result AccountingProblem Account
deduct amount account =
    case account of
        Account balance ->
            if balance >= amount then
                Ok (Account (balance - amount))

            else
                Err InsufficientBalance


hasAtLeast : Money -> Account -> Bool
hasAtLeast amount account =
    case account of
        Account balance ->
            balance >= amount


balanceOf : Account -> Money
balanceOf account =
    case account of
        Account balance ->
            balance
