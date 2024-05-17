module Benzino.Die exposing (..)

import Random


type Face
    = Yek
    | Du
    | Se
    | Jhar
    | Panj
    | Shesh


rollingDie : Random.Generator Face
rollingDie =
    Random.uniform Yek
        [ Du
        , Se
        , Jhar
        , Panj
        , Shesh
        ]


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
