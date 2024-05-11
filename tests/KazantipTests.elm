module KazantipTests exposing (..)

import Aparat exposing (DeterminedEvent)
import Balance exposing (Balance(..))
import Common.Money exposing (Money)
import Expect exposing (..)
import Fuzz exposing (int)
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
                        (Balance.topUp event.payout balanceAfterBet)
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


type StrategyEffect
    = Win Money
    | Lose Money


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
        [ describe "Single Round"
            [ let
                concludeSession { initialBalance, desiredBalance, bet } seed =
                    WorldState [] (Balance initialBalance) seed
                        |> autoplaySafeUntil desiredBalance bet

                accumulateWhenOver : Money -> WorldState -> Stats -> Stats
                accumulateWhenOver threshold { account } earlierStats =
                    if Balance.toMoney account < threshold then
                        { earlierStats | lost = earlierStats.lost + 1 }

                    else
                        { earlierStats | won = earlierStats.won + 1 }
              in
              fuzz int "player may win or lose while sufficient funds" <|
                \salt ->
                    let
                        seed =
                            initialSeed salt

                        ( seedsForIndependentSessions, _ ) =
                            Random.step (Random.list 100 Random.independentSeed) seed

                        independentSessions =
                            seedsForIndependentSessions
                                |> List.map
                                    (concludeSession
                                        { initialBalance = 1000
                                        , desiredBalance = 2000
                                        , bet = 100
                                        }
                                    )

                        stats =
                            independentSessions
                                |> List.foldl
                                    (accumulateWhenOver 2000)
                                    { won = 0, lost = 0 }
                    in
                    stats
                        |> Expect.all
                            [ .won >> Expect.atLeast 35
                            , .lost >> Expect.atLeast 35
                            ]
            ]
        ]


type alias Stats =
    { won : Int
    , lost : Int
    }
