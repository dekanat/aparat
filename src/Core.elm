module Core exposing (..)


type alias Money =
    Int


type Balance
    = Balance Money


topUpBalance : Balance -> Money -> Balance
topUpBalance currentBalance amountToAdd =
    case currentBalance of
        Balance availableAmount ->
            Balance (availableAmount + amountToAdd)
