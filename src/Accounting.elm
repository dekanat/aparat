module Accounting exposing (..)

import Common.Money exposing (Money)
import Time exposing (Weekday(..))


type AccountingOps
    = WithdrawalRequest Money


type AccountingResult
    = WithdrawalSuccess Money
    | WithdrawalFailure


type Exchange
    = Request AccountingOps
    | ToOthers AccountingResult


type Account
    = Account Money


add : Money -> Account -> Account
add amount account =
    case account of
        Account current ->
            Account (current + amount)


type AccountingProblem
    = InsufficientBalance


update : AccountingOps -> Account -> ( Account, Exchange )
update msg account =
    case msg of
        WithdrawalRequest amount ->
            case deduct amount account of
                Ok newAccount ->
                    ( newAccount
                    , ToOthers (WithdrawalSuccess 2000)
                    )

                Err _ ->
                    ( account, ToOthers WithdrawalFailure )


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
    balanceOf account >= amount


balanceOf : Account -> Money
balanceOf account =
    case account of
        Account balance ->
            balance
