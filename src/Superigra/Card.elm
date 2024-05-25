module Superigra.Card exposing (..)

-- Define the rank of a card


type Rank
    = Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace


ranks : List Rank
ranks =
    [ Two
    , Three
    , Four
    , Five
    , Six
    , Seven
    , Eight
    , Nine
    , Ten
    , Jack
    , Queen
    , King
    , Ace
    ]



-- Define the suit of a card


type Suit
    = Hearts
    | Diamonds
    | Clubs
    | Spades


suits : List Suit
suits =
    [ Hearts
    , Diamonds
    , Clubs
    , Spades
    ]



-- Define a card with a rank and a suit


type alias Card =
    { rank : Rank
    , suit : Suit
    }


cardToUnicode : Card -> String
cardToUnicode card =
    let
        baseCodepoint =
            case card.suit of
                Spades ->
                    0x0001F0A0

                Hearts ->
                    0x0001F0B0

                Diamonds ->
                    0x0001F0C0

                Clubs ->
                    0x0001F0D0

        rankOffset =
            case card.rank of
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
                    12

                King ->
                    13
    in
    String.fromChar (Char.fromCode (baseCodepoint + rankOffset))
