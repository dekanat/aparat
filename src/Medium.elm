module Medium exposing (..)

import Account exposing (Account(..))
import Benzino exposing (DiceRoll, benzino)
import Common.Money exposing (Money)
import History
import Round exposing (RoundOutcome, RoundOverview, playRound)
import Session exposing (Session, SessionProblem(..), SessionState)


playBenzino =
    playRound benzino


playOnce : Money -> Session DiceRoll -> Result SessionProblem (Session DiceRoll)
playOnce amountToBet ( aggregates, seed ) =
    let
        settleSessionState accountAfterBet =
            let
                completeRoundOverview : RoundOutcome e -> RoundOverview e
                completeRoundOverview { payout, event } =
                    { bet = amountToBet
                    , seed = seed
                    , payout = payout
                    , details = event
                    }

                evolveState : RoundOverview DiceRoll -> SessionState DiceRoll
                evolveState event =
                    { history = aggregates.history |> History.add event
                    , account = accountAfterBet |> Account.add event.payout
                    }
            in
            seed
                |> playBenzino amountToBet
                |> Tuple.mapFirst (completeRoundOverview >> evolveState)
    in
    aggregates.account
        |> Account.deduct amountToBet
        |> Result.mapError (\_ -> NonRecoverable)
        |> Result.map settleSessionState
