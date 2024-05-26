module Super.CardTests exposing (..)

import Superigra.Card exposing (expectationsFromRegularCards)
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
