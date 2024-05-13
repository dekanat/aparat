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
                                |> History.record ( Account 2000, stubLoseNo 1 )
                                |> History.record ( Account 1000, stubLoseNo 2 )
                                |> History.record ( Account 0, stubWinNo 3 )
                        , account = Account 6000
                        }
                in
                sessionState.history
                    |> List.map
                        (\( accountDuringRound, event ) ->
                            balanceOf accountDuringRound + event.bet
                        )
                    |> Expect.equal [ 3000, 2000, 1000 ]
        ]
