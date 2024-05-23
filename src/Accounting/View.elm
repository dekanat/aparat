module Accounting.View exposing (..)

import Accounting.Account exposing (Account(..))
import Debug exposing (toString)
import Element


balanceDisplay (Account balance) =
    Element.text ("Account Balance: " ++ toString balance)
