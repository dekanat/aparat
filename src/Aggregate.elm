module Aggregate exposing (..)


type alias Lens state sub =
    { get : state -> sub
    , set : sub -> state -> state
    }


type alias Update state request msg =
    request -> state -> ( state, Maybe msg )


type alias UpdateInner state msg =
    state -> ( state, Maybe msg )


performCycleOver : Lens state sub -> UpdateInner sub msg -> state -> ( state, Maybe msg )
performCycleOver lens updateInner state =
    let
        getBoudedState =
            lens.get

        setBoundedState =
            Tuple.mapFirst (\new -> lens.set new state)
    in
    state
        |> getBoudedState
        |> updateInner
        |> setBoundedState
