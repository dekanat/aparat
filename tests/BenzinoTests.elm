module BenzinoTests exposing (..)

import Account exposing (Account(..), AccountingProblem(..))
import Aparat
import Benzino exposing (..)
import Common.Die exposing (Face(..))
import Common.Money exposing (Money)
import Expect exposing (..)
import Fuzz exposing (int)
import History
import Random exposing (initialSeed)
import Result exposing (..)
import Set
import Test exposing (..)
import Time exposing (Weekday(..))


randomSession :
    { a | initialBalance : Money, desiredBalance : Money, betAmount : Money }
    -> Random.Seed
    -> SessionContext
randomSession config seed =
    let
        isGoodToExit ( { account }, _ ) =
            account |> Account.hasAtLeast config.desiredBalance

        loop currentState =
            case Benzino.playOnce config.betAmount currentState of
                Ok evolvedState ->
                    if isGoodToExit evolvedState then
                        evolvedState

                    else
                        loop evolvedState

                Err _ ->
                    currentState

        commonStarterState =
            { history = History.empty
            , account = Account config.initialBalance
            }
    in
    loop
        ( commonStarterState, seed )


compoundTest : Test
compoundTest =
    describe "Game"
        [ describe "Continous sessions"
            [ test "player may win or lose before doubling wealth" <|
                \() ->
                    let
                        manySessions =
                            100

                        fixedSettings =
                            { initialBalance = 1000
                            , desiredBalance = 2000
                            , betAmount = 100
                            }

                        independentSessions =
                            manySessions
                                |> (List.range 1 >> List.map initialSeed)
                                |> List.map (randomSession fixedSettings)
                                |> List.map Tuple.first
                    in
                    -- TODO: imi bereq mi hat
                    independentSessions
                        |> Expect.all
                            [ List.filter (.account >> Account.hasAtLeast 1000)
                                >> List.length
                                >> Expect.all [ Expect.atLeast 25, Expect.atMost 75 ]
                            , List.map (.history >> List.length)
                                >> (List.minimum >> Maybe.withDefault 0)
                                >> Expect.atLeast 2
                            ]
            ]
        , describe "unit functionality"
            [ describe "Round"
                [ fuzz2 int int "should pay based on roll outcome" <|
                    \salt bet ->
                        let
                            ( event, _ ) =
                                initialSeed salt
                                    |> generateOutcome bet
                        in
                        event.payout
                            |> Expect.equal (Aparat.determinPayout bet event.roll)
                , fuzz int "should vary outcome like a normal die" <|
                    \salt ->
                        let
                            currentSeed =
                                initialSeed salt

                            fixedBet =
                                500

                            itr _ ( previousOutcomes, seed ) =
                                let
                                    ( { roll }, nextSeed ) =
                                        generateOutcome fixedBet seed
                                in
                                ( roll :: previousOutcomes, nextSeed )

                            results =
                                List.range 1 1000
                                    |> List.foldl itr ( [], currentSeed )
                                    |> Tuple.first
                        in
                        results
                            |> List.map Debug.toString
                            |> Set.fromList
                            |> Set.size
                            |> Expect.equal 36
                ]
            ]
        ]
