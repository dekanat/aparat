module KazantipTests exposing (..)

import Account exposing (Account(..), AccountingProblem(..))
import Aparat exposing (DeterminedEvent, RandomOutcome)
import Balance exposing (Balance(..))
import Common.Money exposing (Money)
import Expect exposing (..)
import Random exposing (Seed, initialSeed)
import Test exposing (..)


type alias SessionContext =
    { history : List DeterminedEvent
    , account : Account
    , seed : Seed
    }


type SessionProblem
    = NonRecoverable


play : Money -> SessionContext -> Result SessionProblem SessionContext
play amountToBet { seed, history, account } =
    let
        playSafe : Account -> SessionContext
        playSafe safelyDeduced =
            let
                updateCurrentContext : RandomOutcome -> SessionContext
                updateCurrentContext ( event, nextSeed ) =
                    SessionContext
                        (event :: history)
                        (safelyDeduced |> Account.add event.payout)
                        nextSeed
            in
            amountToBet
                |> Aparat.playRound seed
                |> updateCurrentContext
    in
    account
        |> Account.deduct amountToBet
        |> Result.map playSafe
        |> Result.mapError (\_ -> NonRecoverable)


autoplaySafeUntil : Money -> Money -> SessionContext -> SessionContext
autoplaySafeUntil desiredBalance betAmount initialWorld =
    let
        isGoodEnough =
            .account >> Account.hasAtLeast desiredBalance

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
                    SessionContext [] (Account initialBalance) seed
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
        ]
