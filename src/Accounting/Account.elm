module Accounting.Account exposing (..)

import Common.Money exposing (Money)


type Account
    = Account Money


type AccountingProblem
    = InsufficientBalance


add : Money -> Account -> Account
add amount (Account balance) =
    Account (balance + amount)


deduct : Money -> Account -> Result AccountingProblem Account
deduct amount (Account balance) =
    if balance >= amount then
        Ok (Account (balance - amount))

    else
        Err InsufficientBalance
