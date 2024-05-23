module Aggregate exposing (..)


type alias Lens state sub =
    { get : state -> sub
    , set : sub -> state -> state
    }


type alias Aggregate state sub req msg =
    { lens : Lens state sub
    , update : req -> sub -> ( sub, msg )
    }


type alias Update sub req msg =
    req -> sub -> ( sub, msg )


performCycle : Lens state sub -> Update sub req msg -> req -> state -> ( state, msg )
performCycle lens update req state =
    let
        getBoudedState =
            lens.get

        setBoundedState =
            Tuple.mapFirst (\new -> lens.set new state)
    in
    state
        |> getBoudedState
        |> update req
        |> setBoundedState
