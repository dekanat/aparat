module BenzinoTests exposing (..)

import Account exposing (Account(..), AccountingProblem(..))
import Aparat
import Benzino exposing (..)
import Common.Die exposing (Face(..))
import Common.Money exposing (Money)
import Expect exposing (..)
import Fuzz exposing (int)
import Random exposing (initialSeed)
import Result exposing (..)
import Set
import Test exposing (..)


autoplaySafeUntil : Money -> Money -> SessionContext -> SessionContext
autoplaySafeUntil desiredBalance betAmount initialWorld =
    let
        isGoodEnough session =
            case session of
                SettledSession { account } _ ->
                    account |> Account.hasAtLeast desiredBalance

        loop currentWorld =
            case currentWorld |> Benzino.playOnce betAmount of
                Ok newWorld ->
                    if isGoodEnough newWorld then
                        newWorld

                    else
                        loop newWorld

                Err _ ->
                    currentWorld
    in
    initialWorld |> loop


compoundTest : Test
compoundTest =
    describe "Game"
        [ describe "Continous sessions"
            [ let
                randomSession { initialBalance, desiredBalance, bet } seed =
                    SettledSession
                        { history = []
                        , account = Account initialBalance
                        }
                        seed
                        |> autoplaySafeUntil desiredBalance bet

                extractAggregates session =
                    case session of
                        SettledSession aggregates _ ->
                            aggregates
              in
              test "player may win or lose before doubling wealth" <|
                \() ->
                    let
                        seedsForIndependentSessions =
                            List.range 1 100
                                |> List.map initialSeed

                        independentSessions =
                            seedsForIndependentSessions
                                |> List.map
                                    (randomSession
                                        { initialBalance = 1000
                                        , desiredBalance = 2000
                                        , bet = 100
                                        }
                                    )
                                |> List.map extractAggregates
                    in
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
