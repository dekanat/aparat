module Control.Control exposing (..)

import Aggregate
import Common.Money exposing (Money)
import Control.Account as Account exposing (Account(..))
import List.Extra


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
    = Bet Money


type alias CallbackInterface msg =
    { placeBet : Money -> msg
    }


updateWith : CallbackInterface msg -> Aggregate.Update State Request msg
updateWith { placeBet } request state =
    case request of
        Bet amount ->
            case state.account |> Account.deduct amount of
                Ok reducedAccount ->
                    ( { state | account = reducedAccount }
                    , Just (placeBet amount)
                    )

                Err _ ->
                    ( state, Nothing )



-- init : Money -> State
-- init amount =
--     Account amount
-- type alias Callback msg =
--     { fulfillOrder : Money -> msg
--     , rejectOrder : msg
--     }
-- updateWith : Callback msg -> Aggregate.Update State Request msg
-- updateWith { fulfillOrder, rejectOrder } request model =
--     case request of
--         Withdraw money ->
--             let
--                 deduction =
--                     Account.deduct money model
--             in
--             case deduction of
--                 Ok account ->
--                     ( account, Just (fulfillOrder money) )
--                 Err _ ->
--                     ( model, Just rejectOrder )
--         Replenish money ->
--             let
--                 newModel =
--                     Account.add money model
--             in
--             ( newModel, Nothing )
