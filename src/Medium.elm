module Medium exposing (..)

import Account exposing (Account(..))
import Benzino.Benzino exposing (DiceRoll, benzino)
import Common.Money exposing (Money)
import History
import Round exposing (playRound)
import Session exposing (Session, SessionProblem(..))


playOnce : Money -> Session DiceRoll -> Result SessionProblem (Session DiceRoll)
playOnce amountToBet ( aggregates, seed ) =
    let
        settleSessionState accountAfterBet =
            let
                completeRoundOverview { payout, event } =
                    { bet = amountToBet
                    , seed = seed
                    , payout = payout
                    , details = event
                    }

                evolveState event =
                    { history = aggregates.history |> History.add event
                    , account = accountAfterBet |> Account.add event.payout
                    }
            in
            seed
                |> playRound benzino amountToBet
                |> Tuple.mapFirst (completeRoundOverview >> evolveState)
    in
    aggregates.account
        |> Account.deduct amountToBet
        |> Result.mapError (\_ -> NonRecoverable)
        |> Result.map settleSessionState
