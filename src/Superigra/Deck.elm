module Superigra.Deck exposing (..)

import Expect
import List.Extra
import Random
import Random.List exposing (choices)
import Superigra.Card as Card exposing (Card)
import Test exposing (Test, describe, test)


type alias Deck =
    List Card


freshDeck : Deck
freshDeck =
    Card.suits
        |> List.Extra.andThen
            (\suit ->
                Card.ranks
                    |> List.Extra.andThen (\rank -> [ Card rank suit ])
            )


freshDeckTests : Test
freshDeckTests =
    describe "Deck"
        [ test """ Should be standard 52-card deck

            🂢 🂣 🂤 🂥 🂦 🂧 🂨 🂩 🂪 🂫 🂭 🂮 🂡
            🂲 🂳 🂴 🂵 🂶 🂷 🂸 🂹 🂺 🂻 🂽 🂾 🂱
            🃂 🃃 🃄 🃅 🃆 🃇 🃈 🃉 🃊 🃋 🃍 🃎 🃁
            🃒 🃓 🃔 🃕 🃖 🃗 🃘 🃙 🃚 🃛 🃝 🃞 🃑
        """ <|
            \() ->
                freshDeck
                    |> Expect.all
                        [ List.length >> Expect.equal 52
                        , List.Extra.unique >> List.length >> Expect.equal 52
                        ]
        ]


shuffleAndDeal : Int -> Deck -> ( List Card, Deck )
shuffleAndDeal count deck =
    let
        seed =
            Random.initialSeed 0

        gen =
            choices count deck

        ( res, _ ) =
            Random.step gen seed
    in
    res
