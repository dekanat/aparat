module Accounting.Accounting exposing (..)

import Accounting.Account as Account exposing (Account(..))
import Aggregate
import Common.Money exposing (Money)
import Time exposing (Weekday(..))


type Request
    = Withdraw Money
    | Replenish Money


type alias State =
    Account


init : Money -> State
init amount =
    Account amount


type alias Callback msg =
    { fulfillOrder : Money -> msg
    , rejectOrder : msg
    }


updateWith : Callback msg -> Aggregate.Update State Request msg
updateWith { fulfillOrder, rejectOrder } request model =
    case request of
        Withdraw money ->
            let
                deduction =
                    Account.deduct money model
            in
            case deduction of
                Ok account ->
                    ( account, Just (fulfillOrder money) )

                Err _ ->
                    ( model, Just rejectOrder )

        Replenish money ->
            let
                newModel =
                    Account.add money model
            in
            ( newModel, Nothing )
