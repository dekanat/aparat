module Dublich.CardTests exposing (..)

import Dublich.Card exposing (expectationsFromRegularCards)
import Test exposing (Test, describe, test)


regularCardsTests : Test
regularCardsTests =
    describe "Deck"
        [ expectationsFromRegularCards
            |> always
            |> test """
                regular 52 cards

                🂢 🂣 🂤 🂥 🂦 🂧 🂨 🂩 🂪 🂫 🂭 🂮 🂡
                🂲 🂳 🂴 🂵 🂶 🂷 🂸 🂹 🂺 🂻 🂽 🂾 🂱
                🃂 🃃 🃄 🃅 🃆 🃇 🃈 🃉 🃊 🃋 🃍 🃎 🃁
                🃒 🃓 🃔 🃕 🃖 🃗 🃘 🃙 🃚 🃛 🃝 🃞 🃑
            """
        ]
