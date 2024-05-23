module Accounting.Accounting exposing (..)

import Common.Money exposing (Money)
import Time exposing (Weekday(..))


type Request
    = Withdraw Money
    | Replenish Money


type Account
    = Account Money


add : Money -> Account -> Account
add amount account =
    case account of
        Account current ->
            Account (current + amount)


type AccountingProblem
    = InsufficientBalance


type alias Model =
    Account


type alias Callback msg =
    { fulfillOrder : Money -> msg
    , rejectOrder : msg
    }


updateWith : Callback msg -> Request -> Model -> ( Model, Maybe msg )
updateWith { fulfillOrder, rejectOrder } request model =
    case request of
        Withdraw money ->
            let
                deduction =
                    deduct money model
            in
            case deduction of
                Ok account ->
                    ( account, Just (fulfillOrder money) )

                Err _ ->
                    ( model, Just rejectOrder )

        Replenish money ->
            let
                newAccount =
                    add money model
            in
            ( newAccount, Nothing )


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
