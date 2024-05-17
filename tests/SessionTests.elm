module SessionTests exposing (..)

import Account exposing (Account(..))
import Benzino.Benzino exposing (DiceRoll)
import Benzino.Die exposing (Face(..))
import Common.Money exposing (Money)
import Expect exposing (..)
import History
import List.Extra
import Random exposing (initialSeed)
import Round exposing (RoundOverview)
import Session exposing (SessionState)
import SessionPlot
import Test exposing (..)


sessionOperations : Test
sessionOperations =
    describe "Session Operations"
        [ test "replay past events" <|
            \() ->
                let
                    stubLoseNo : Int -> RoundOverview DiceRoll
                    stubLoseNo idx =
                        { seed = initialSeed idx
                        , details = ( Yek, Du )
                        , bet = 1000
                        , payout = 0
                        }

                    stubWinNo idx =
                        { seed = initialSeed idx
                        , details = ( Yek, Yek )
                        , bet = 1000
                        , payout = 6000
                        }

                    sessionState : Session.SessionState DiceRoll
                    sessionState =
                        { history =
                            History.empty
                                |> History.add (stubLoseNo 1)
                                |> History.add (stubLoseNo 2)
                                |> History.add (stubWinNo 3)
                        , account = Account 6000
                        }

                    replay : (( Money, RoundOverview a ) -> b) -> SessionState a -> List b
                    replay mapper currentState =
                        let
                            adjustBalanceDynamics ( balanceBeforeEvent, event ) =
                                ( balanceBeforeEvent - event.bet, event )

                            mixedSequence =
                                currentState.history
                                    |> List.Extra.zip (SessionPlot.balanceSettledThrough currentState)
                                    |> List.map adjustBalanceDynamics
                        in
                        mixedSequence
                            |> List.map mapper
                in
                sessionState
                    |> replay
                        (\( balanceDuringEvent, event ) ->
                            balanceDuringEvent + event.bet
                        )
                    |> Expect.equal [ 3000, 2000, 1000 ]
        ]
