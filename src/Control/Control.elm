module Control.Control exposing (..)

import Aggregate
import Common.Money exposing (Money)
import Control.Account as Account exposing (Account(..))


type alias State =
    { account : Account
    , betOptions : List Money
    , selectedBet : Maybe Money
    }


init : Money -> List Money -> State
init balance betOptions =
    { account = Account balance
    , betOptions = betOptions
    , selectedBet = betOptions |> List.minimum
    }


type Request
    = OrderBet Money
    | ReplenishAccount Money


type alias CallbackInterface msg =
    { betPlaced : Money -> msg
    }


updateWith : CallbackInterface msg -> Aggregate.Update State Request msg
updateWith { betPlaced } request state =
    case request of
        OrderBet amount ->
            case state.account |> Account.deduct amount of
                Ok reducedAccount ->
                    ( { state | account = reducedAccount }
                    , Just (betPlaced amount)
                    )

                Err _ ->
                    ( state, Nothing )

        ReplenishAccount amount ->
            let
                newAccount =
                    state.account |> Account.add amount
            in
            ( { state | account = newAccount }, Nothing )
