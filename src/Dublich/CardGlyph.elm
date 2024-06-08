module Dublich.CardGlyph exposing (..)

import Dublich.Card exposing (Card(..), Rank(..), Suit(..))


cardSymbol : Card -> String
cardSymbol card =
    let
        codePoint =
            case card of
                RegularCard suit rank ->
                    codeForRegular suit rank

                Joker ->
                    codeForJoker
    in
    String.fromChar (Char.fromCode codePoint)


codeForJoker : number
codeForJoker =
    0x0001F0CF


codeForRegular : Suit -> Rank -> Int
codeForRegular suit rank =
    let
        baseCodepoint =
            case suit of
                Spades ->
                    0x0001F0A0

                Hearts ->
                    0x0001F0B0

                Diamonds ->
                    0x0001F0C0

                Clubs ->
                    0x0001F0D0

        rankOffset =
            case rank of
                Ace ->
                    1

                Two ->
                    2

                Three ->
                    3

                Four ->
                    4

                Five ->
                    5

                Six ->
                    6

                Seven ->
                    7

                Eight ->
                    8

                Nine ->
                    9

                Ten ->
                    10

                Jack ->
                    11

                Queen ->
                    13

                King ->
                    14
    in
    baseCodepoint + rankOffset


cardBackSymbol : String
cardBackSymbol =
    String.fromChar (Char.fromCode codeForCardBack)


codeForCardBack : number
codeForCardBack =
    0x0001F0A0
