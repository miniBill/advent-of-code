module Y2023.Day3 exposing (gold, silver)

import Dict exposing (Dict)
import Utils


silver : String -> Result String String
silver input =
    let
        grid : Dict Int (Dict Int Char)
        grid =
            toGrid input
    in
    grid
        |> Dict.foldl
            (\row line acc ->
                acc + 0
            )
            0
        |> String.fromInt
        |> Ok


toGrid : String -> Dict Int (Dict Int Char)
toGrid input =
    input
        |> Utils.lines
        |> List.map (Dict.fromList << List.indexedMap Tuple.pair << String.toList)
        |> List.indexedMap Tuple.pair
        |> Dict.fromList


gold : String -> Result String String
gold =
    Utils.perLineWith (\_ -> Err "TODO") Ok
