module Card exposing (..)

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



-- Define the suit of a card


type Suit
    = Hearts
    | Diamonds
    | Clubs
    | Spades



-- Define a card with a rank and a suit


type alias Card =
    { rank : Rank
    , suit : Suit
    }
