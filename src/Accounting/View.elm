module Accounting.View exposing (..)

import Accounting.Account exposing (Account(..))
import Debug exposing (toString)
import Element


balanceDisplay { currentBalance } =
    Element.text ("Account Balance: " ++ toString currentBalance)
