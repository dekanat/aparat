module Benzino.View exposing (..)

import Benzino.Benzino exposing (DiceRoll)
import Benzino.Die exposing (Face(..))
import Element
import Element.Font


glyphFor : Face -> String
glyphFor face =
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


type alias DisplaySettings =
    { size : Int
    }


benzinoResultsDisplay : DisplaySettings -> Maybe DiceRoll -> Element.Element msg
benzinoResultsDisplay { size } maybeEvent =
    let
        pictogramFor : ( Face, Face ) -> Element.Element msg
        pictogramFor ( rolledA, rolledB ) =
            Element.row
                [ Element.Font.size size
                , Element.spacing 8
                ]
                [ Element.text (glyphFor rolledA)
                , Element.text (glyphFor rolledB)
                ]
    in
    case maybeEvent of
        Nothing ->
            pictogramFor ( Shesh, Yek )

        Just event ->
            pictogramFor event
