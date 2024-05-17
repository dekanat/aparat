module SessionPlotTests exposing (..)

import Account exposing (Account(..))
import Benzino.Die exposing (Face(..))
import Expect exposing (..)
import History
import Random exposing (initialSeed)
import Session exposing (SessionState)
import SessionPlot
import Test exposing (..)


sessionOperations : Test
sessionOperations =
    describe "Session Operations"
        [ describe "balance throughout session"
            [ test "empty history" <|
                \() ->
                    SessionState History.empty (Account 10000)
                        |> SessionPlot.balanceSettledThrough
                        |> Expect.equal [ 10000 ]
            , test "rich history of events" <|
                \() ->
                    let
                        richHistory =
                            History.empty
                                |> History.add { seed = initialSeed 1, bet = 1000, payout = 0, details = ( Yek, Du ) }
                                |> History.add { seed = initialSeed 2, bet = 1000, payout = 0, details = ( Yek, Du ) }
                                |> History.add { seed = initialSeed 3, bet = 1000, payout = 6000, details = ( Yek, Yek ) }
                    in
                    SessionState richHistory (Account 10000)
                        |> SessionPlot.balanceSettledThrough
                        |> Expect.equal [ 7000, 6000, 5000, 10000 ]
            ]
        ]
