module KazantipTests exposing (..)

import Aparat exposing (DeterminedEvent)
import Balance exposing (Balance(..))
import Common.Money exposing (Money)
import Expect exposing (..)
import Random exposing (Seed, initialSeed)
import Test exposing (..)


type alias PlayerAccount =
    Balance


type alias WorldState =
    { history : List DeterminedEvent
    , account : PlayerAccount
    , seed : Seed
    }


type PlayingProblem
    = BalanceProblem


play : Money -> WorldState -> Result PlayingProblem WorldState
play amountToBet { seed, history, account } =
    let
        playSafe : PlayerAccount -> WorldState
        playSafe balanceAfterBet =
            let
                applyToWorld ( event, nextSeed ) =
                    WorldState
                        (event :: history)
                        (balanceAfterBet |> Balance.topUp event.payout)
                        nextSeed
            in
            amountToBet
                |> Aparat.playRound seed
                |> applyToWorld
    in
    account
        |> Balance.take amountToBet
        |> Result.map playSafe
        |> Result.mapError (\_ -> BalanceProblem)


autoplaySafeUntil : Money -> Money -> WorldState -> WorldState
autoplaySafeUntil desiredBalance betAmount initialWorld =
    let
        isGoodEnough newWorldOrder =
            Balance.toMoney newWorldOrder.account >= desiredBalance

        loop currentWorld =
            case currentWorld |> play betAmount of
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
                    WorldState [] (Balance initialBalance) seed
                        |> autoplaySafeUntil desiredBalance bet
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

                        playerWinsOver : Money -> WorldState -> Int -> Int
                        playerWinsOver desiredBalance { account } count =
                            if Balance.toMoney account > desiredBalance then
                                count + 1

                            else
                                count
                    in
                    independentSessions
                        |> Expect.all
                            [ List.foldl (playerWinsOver 1000) 0
                                >> Expect.all [ Expect.atLeast 25, Expect.atMost 75 ]
                            , List.map (.history >> List.length)
                                >> (List.minimum >> Maybe.withDefault 0)
                                >> Expect.atLeast 2
                            ]
            ]
        ]
