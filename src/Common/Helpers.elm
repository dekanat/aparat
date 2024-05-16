module Common.Helpers exposing (..)


slidingWindow : Int -> List a -> List (List a)
slidingWindow size initialList =
    let
        slide list =
            if List.length list < size then
                []

            else
                List.take size list :: slide (List.drop 1 list)
    in
    if size <= 0 then
        []

    else
        slide initialList


slidingPair : List a -> List ( a, a )
slidingPair list =
    list
        |> slidingWindow 2
        |> List.filterMap
            (\window ->
                case window of
                    [ a, b ] ->
                        Just ( a, b )

                    _ ->
                        Nothing
            )
