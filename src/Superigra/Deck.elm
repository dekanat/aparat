module Superigra.Deck exposing (..)

import Expect
import List.Extra
import Random
import Random.List exposing (choices)
import Superigra.Card as Card exposing (Card(..))
import Test exposing (Test, describe, test)


type alias Deck =
    List Card


regularCards : List Card
regularCards =
    Card.suits
        |> List.Extra.andThen
            (\suit ->
                Card.ranks
                    |> List.Extra.andThen (\rank -> [ RegularCard suit rank ])
            )


regularCardsTests : Test
regularCardsTests =
    describe "Deck"
        [ test """ Should be standard 52-card deck

            🂢 🂣 🂤 🂥 🂦 🂧 🂨 🂩 🂪 🂫 🂭 🂮 🂡
            🂲 🂳 🂴 🂵 🂶 🂷 🂸 🂹 🂺 🂻 🂽 🂾 🂱
            🃂 🃃 🃄 🃅 🃆 🃇 🃈 🃉 🃊 🃋 🃍 🃎 🃁
            🃒 🃓 🃔 🃕 🃖 🃗 🃘 🃙 🃚 🃛 🃝 🃞 🃑
        """ <|
            \() ->
                regularCards
                    |> Expect.all
                        [ List.length >> Expect.equal 52
                        , List.Extra.unique >> List.length >> Expect.equal 52
                        ]
        ]
