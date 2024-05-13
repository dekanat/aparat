module Benzino.PayoutLogicTests exposing (..)

import Aparat
import Common.Die exposing (Face(..))
import Expect exposing (..)
import Fuzz exposing (..)
import Test exposing (..)


aparatTests : Test
aparatTests =
    describe "Aparat"
        [ describe "Determine Payout"
            [ test "that apprat notifies of wins" <|
                \() ->
                    Aparat.calculatePayout 100 ( Panj, Panj )
                        |> Expect.equal 600
            , test "that apprat notifies of lose" <|
                \() ->
                    Aparat.calculatePayout 100 ( Panj, Yek )
                        |> Expect.equal 0
            ]
        ]
