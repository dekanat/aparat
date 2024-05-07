module UnsortedTests exposing (..)

import Expect exposing (..)
import Test exposing (..)


type alias Money =
    Int


type alias Face =
    Int


type alias Juxt =
    ( Face, Face )


type GameResult
    = PlayerWins Int
    | CasinoWins Int


spin : Money -> Juxt -> GameResult
spin stake ( a, b ) =
    if a == b then
        PlayerWins (stake * a)

    else
        CasinoWins stake


juxtZarTests1 : Test
juxtZarTests1 =
    let
        testGenerator : Int -> Test
        testGenerator idx =
            test
                ("Juxt wins x"
                    ++ String.fromInt idx
                    ++ " on "
                    ++ String.fromInt idx
                    ++ ":"
                    ++ String.fromInt idx
                )
            <|
                \() ->
                    spin 100 ( idx, idx )
                        |> Expect.equal (PlayerWins (100 * idx))
    in
    describe "juxt zar apparatusss"
        [ describe "single wins"
            [ describe "wins"
                (List.map testGenerator (List.range 1 6))
            , describe "lose"
                [ test "No Juxt no money x" <|
                    \() ->
                        spin 100 ( 1, 2 )
                            |> Expect.equal (CasinoWins 100)
                ]
            ]
        , describe "cumulative effect"
            [ todo "check 10 plays"
            , todo "check 100 x 100 plays"
            ]
        ]
