module Die exposing (..)

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


pictogramFor : Face -> String
pictogramFor dieFace =
    case dieFace of
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
