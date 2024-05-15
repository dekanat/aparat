module HelpersTests exposing (..)

import Common.Helpers exposing (slidingWindow)
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Sliding Window"
        [ test "Extract sliding windows of a specified size" <|
            \() ->
                [ 1, 2, 3, 4 ]
                    |> Expect.all
                        [ slidingWindow 2 >> Expect.equal [ [ 1, 2 ], [ 2, 3 ], [ 3, 4 ] ]
                        , slidingWindow 3 >> Expect.equal [ [ 1, 2, 3 ], [ 2, 3, 4 ] ]
                        ]
        , test "Corner cases" <|
            \() ->
                [ 1, 2, 3, 4 ]
                    |> Expect.all
                        [ slidingWindow 5 >> Expect.equal []
                        , slidingWindow 0 >> Expect.equal []
                        , slidingWindow -1 >> Expect.equal []
                        ]
        ]
