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


expectationsFromRegularCards : Expectation
expectationsFromRegularCards =
    regularCards
        |> Expect.all
            [ List.length >> Expect.equal 52
            , List.Extra.unique >> List.length >> Expect.equal 52
            ]
