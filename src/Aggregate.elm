module Aggregate exposing (..)


type alias Lens state sub =
    { get : state -> sub
    , set : sub -> state -> state
    }


type alias Update sub req msg =
    req -> sub -> ( sub, Maybe msg )


performCycle : Lens state sub -> Update sub req msg -> req -> state -> ( state, Maybe msg )
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
