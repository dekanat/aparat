module Aparat.DisplayRound exposing (..)

import Aparat.PairOfDice exposing (DieFace(..), PossibleCombination)
import Element
import Element.Font


symbolFor : DieFace -> String
symbolFor face =
    case face of
        Yek ->
            "⚀"

        Du ->
            "⚁"

        Se ->
            "⚂"

        Jhar ->
            "⚃"

        Panj ->
            "⚄"

        Shesh ->
            "⚅"


view : Maybe PossibleCombination -> Element.Element msg
view lastEvent =
    let
        xxlSize =
            200

        defaultCombination =
            ( Shesh, Yek )

        pictogramFor : ( DieFace, DieFace ) -> Element.Element msg
        pictogramFor ( rolledA, rolledB ) =
            Element.row
                [ Element.Font.size xxlSize
                , Element.spacing 8
                ]
                [ Element.text (symbolFor rolledA)
                , Element.text (symbolFor rolledB)
                ]
    in
    case lastEvent of
        Nothing ->
            pictogramFor defaultCombination

        Just settledCombination ->
            pictogramFor settledCombination
