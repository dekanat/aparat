module SessionTests exposing (..)

import Account exposing (Account(..), balanceOf)
import Aparat exposing (DiceRoll)
import Common.Die exposing (Face(..))
import Common.Money exposing (Money)
import Expect exposing (..)
import History
import List.Extra
import Random exposing (initialSeed)
import Round exposing (Round)
import Session exposing (SessionState)
import Test exposing (..)


sessionOperations : Test
sessionOperations =
    describe "Session Operations"
        [ test "replay past events" <|
            \() ->
                let
                    stubLoseNo : Int -> Round DiceRoll
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

                    balanceThroughout : SessionState a -> List Money
                    balanceThroughout { history, account } =
                        let
                            recoverEarlier event balanceAfterEvent =
                                balanceAfterEvent - event.payout + event.bet
                        in
                        history |> List.Extra.scanr recoverEarlier (balanceOf account)

                    replay : (( Money, Round a ) -> b) -> SessionState a -> List b
                    replay mapper currentState =
                        let
                            adjustBalanceDynamics ( balanceBeforeEvent, event ) =
                                ( balanceBeforeEvent - event.bet, event )

                            mixedSequence =
                                currentState.history
                                    |> List.Extra.zip (balanceThroughout currentState)
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
