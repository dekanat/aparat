module Dublich.Card exposing (..)

import Expect exposing (Expectation)
import List.Extra



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


score : Rank -> Int
score rank =
    case rank of
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

        Ace ->
            14


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


type Card
    = RegularCard Suit Rank
    | Joker


regularCards : List Card
regularCards =
    suits
        |> List.Extra.andThen
            (\suit ->
                ranks
                    |> List.Extra.andThen (\rank -> [ RegularCard suit rank ])
            )


compare : Card -> Card -> Order
compare card1 card2 =
    case ( card1, card2 ) of
        ( Joker, _ ) ->
            GT

        ( _, Joker ) ->
            LT

        ( RegularCard _ rank1, RegularCard _ rank2 ) ->
            Basics.compare (score rank1) (score rank2)


expectationsFromRegularCards : Expectation
expectationsFromRegularCards =
    regularCards
        |> Expect.all
            [ List.length >> Expect.equal 52
            , List.Extra.unique >> List.length >> Expect.equal 52
            ]
