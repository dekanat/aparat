module KazantipTests exposing (..)

import Aparat exposing (DeterminedEvent, DiceRoll)
import Balance exposing (Balance(..))
import Common.Money exposing (Money)
import Expect exposing (..)
import Fuzz exposing (int)
import Player
import Random exposing (Seed, initialSeed)
import Test exposing (..)


type alias PlayerModel =
    { balance : Balance
    }


type alias AparatModel =
    { seed : Seed
    , history : List DeterminedEvent
    }


type alias WorldState =
    { player : PlayerModel
    , aparat : AparatModel
    }


type PlayingProblem
    = BalanceProblem


play : Money -> WorldState -> Result PlayingProblem WorldState
play amountToBet { player, aparat } =
    let
        playRound : Seed -> Money -> ( DeterminedEvent, Seed )
        playRound seed bet =
            let
                resolveEvent : DiceRoll -> DeterminedEvent
                resolveEvent roll =
                    roll
                        |> Aparat.resolvePayout bet
                        |> DeterminedEvent seed bet roll
            in
            seed
                |> Random.step Aparat.rollingPairOfDice
                |> Tuple.mapFirst resolveEvent

        initiateRound : ( Money, Balance ) -> WorldState
        initiateRound ( bet, balanceAfterBet ) =
            let
                resolveEvent : DiceRoll -> DeterminedEvent
                resolveEvent roll =
                    roll
                        |> Aparat.resolvePayout bet
                        |> DeterminedEvent aparat.seed bet roll

                playerAfter ( event, _ ) =
                    PlayerModel
                        (Balance.topUp balanceAfterBet event.payout)

                aparatAfter ( event, nextSeed ) =
                    AparatModel
                        nextSeed
                        (event :: aparat.history)
            in
            aparat.seed
                |> Random.step Aparat.rollingPairOfDice
                |> Tuple.mapFirst resolveEvent
                |> (\resolution ->
                        WorldState
                            (playerAfter resolution)
                            (aparatAfter resolution)
                   )
    in
    player.balance
        |> Player.makeBet amountToBet
        |> Result.map initiateRound
        |> Result.mapError (\_ -> BalanceProblem)


compoundTest : Test
compoundTest =
    describe "Game"
        [ describe "Single Round"
            [ fuzz int "player may win or lose while sufficient funds" <|
                \salt ->
                    let
                        currentSeed =
                            initialSeed salt

                        initialWorld =
                            WorldState
                                (PlayerModel (Balance 3000))
                                (AparatModel currentSeed [])

                        playManyRounds : Int -> Money -> WorldState -> WorldState
                        playManyRounds maxTimes amountToBet world =
                            let
                                loop moreTimes currentWorld =
                                    case currentWorld |> play amountToBet of
                                        Ok newWorld ->
                                            if moreTimes > 0 then
                                                loop (moreTimes - 1) newWorld

                                            else
                                                currentWorld

                                        Err _ ->
                                            currentWorld
                            in
                            world |> loop maxTimes
                    in
                    initialWorld
                        |> playManyRounds 100 100
                        |> Expect.all
                            [ (.aparat >> .history) >> List.length >> Expect.atMost 100

                            -- , (.player >> .balance) >> Expect.equal (Balance 0)
                            ]
            ]
        ]
