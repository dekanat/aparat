module Superigra.Deck exposing (..)

import Expect
import List.Extra
import Superigra.Card as Card exposing (Card)
import Test exposing (Test, describe, test)


type alias Deck =
    List Card


deck : Deck
deck =
    Card.suits
        |> List.Extra.andThen
            (\suit ->
                Card.ranks
                    |> List.Extra.andThen (\rank -> [ Card rank suit ])
            )


deckTests : Test
deckTests =
    describe "Deck"
        [ test """ Should be standard 52-card deck

            🂢 🂣 🂤 🂥 🂦 🂧 🂨 🂩 🂪 🂫 🂭 🂮 🂡
            🂲 🂳 🂴 🂵 🂶 🂷 🂸 🂹 🂺 🂻 🂽 🂾 🂱
            🃂 🃃 🃄 🃅 🃆 🃇 🃈 🃉 🃊 🃋 🃍 🃎 🃁
            🃒 🃓 🃔 🃕 🃖 🃗 🃘 🃙 🃚 🃛 🃝 🃞 🃑
        """ <|
            \() ->
                deck
                    |> Expect.all
                        [ List.length >> Expect.equal 52
                        , List.Extra.unique >> List.length >> Expect.equal 52
                        ]
        ]
