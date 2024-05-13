module SessionTests exposing (..)

import Account exposing (Account(..))
import Aparat exposing (DiceRoll)
import Common.Die exposing (Face(..))
import Common.Money exposing (Money)
import Expect exposing (..)
import History exposing (DeterminedEvent)
import List.Extra
import Random exposing (initialSeed)
import Session exposing (SessionState)
import Test exposing (..)


sessionOperations : Test
sessionOperations =
    describe "Session Operations"
        [ test "replay past events" <|
            \() ->
                let
                    stubLoseNo : Int -> DeterminedEvent DiceRoll
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

                    replay : (( Money, DeterminedEvent a ) -> b) -> SessionState a -> List b
                    replay mapper { account, history } =
                        let
                            balance =
                                6000

                            balanceFlowBeforeBets =
                                history
                                    |> List.Extra.scanl
                                        (\event balanceAfterEvent ->
                                            balanceAfterEvent - event.payout + event.bet
                                        )
                                        balance

                            applyMapper : DeterminedEvent a -> b
                            applyMapper event =
                                mapper ( balance, event )
                        in
                        history
                            |> List.map applyMapper

                    result =
                        sessionState
                            |> Expect.all
                                [ replay (\( _, event ) -> event.payout > 0)
                                    >> Expect.equalLists [ False, False, True ]
                                , replay (\( balanceDuringEvent, event ) -> balanceDuringEvent + event.bet)
                                    >> Expect.equalLists [ 3000, 2000, 1000 ]
                                ]
                in
                sessionState.history
                    |> List.Extra.scanr
                        (\event balanceAfterEvent ->
                            balanceAfterEvent - event.payout + event.bet
                        )
                        6000
                    |> Expect.equal [ 3000, 2000, 1000, 6000 ]
        ]
