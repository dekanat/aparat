module Accounting.Accounting exposing (..)

import Accounting.Account as Account exposing (Account(..))
import Aggregate
import Common.Money exposing (Money)
import Time exposing (Weekday(..))


type Request
    = Withdraw Money
    | Replenish Money


type alias State =
    { currentBalance : Money }


init : Money -> State
init amount =
    { currentBalance = amount }


type alias Callback msg =
    { fulfillOrder : Money -> msg
    , rejectOrder : msg
    }


updateWith : Callback msg -> Aggregate.Update State Request msg
updateWith { fulfillOrder, rejectOrder } request { currentBalance } =
    case request of
        Withdraw money ->
            if currentBalance >= money then
                ( { currentBalance = currentBalance - money }
                , Just (fulfillOrder money)
                )

            else
                ( { currentBalance = currentBalance }
                , Just rejectOrder
                )

        Replenish money ->
            ( { currentBalance = currentBalance + money }
            , Nothing
            )
