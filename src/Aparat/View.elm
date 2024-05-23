module Aparat.View exposing (..)

import Aparat.ChanceMechanism exposing (PossibleCombination)
import Aparat.Die as Die exposing (Face(..))
import Element
import Element.Font


symbolFor : Die.Face -> String
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


pictogramFor : PossibleCombination -> Element.Element msg
pictogramFor ( rolledA, rolledB ) =
    Element.row
        [ Element.Font.size 200
        , Element.spacing 8
        ]
        [ Element.text (symbolFor rolledA)
        , Element.text (symbolFor rolledB)
        ]


gameScene : { a | lastEvent : Maybe PossibleCombination } -> Element.Element msg
gameScene { lastEvent } =
    let
        exampleCombination =
            ( Shesh, Yek )
    in
    pictogramFor (lastEvent |> Maybe.withDefault exampleCombination)
