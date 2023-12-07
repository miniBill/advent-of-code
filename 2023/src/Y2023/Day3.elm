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
            (\rowIndex currentRow acc ->
                let
                    previousRow =
                        Dict.get (rowIndex - 1) grid
                            |> Maybe.withDefault Dict.empty

                    nextRow =
                        Dict.get (rowIndex + 1) grid
                            |> Maybe.withDefault Dict.empty
                in
                currentRow
                    |> Dict.insert (Dict.size currentRow) '.'
                    |> Dict.foldl
                        (\colIndex currentChar ( iacc, state ) ->
                            let
                                digit =
                                    String.toInt (String.fromChar currentChar)

                                isSymbolIn col row =
                                    let
                                        str =
                                            row
                                                |> Dict.get col
                                                |> Maybe.withDefault '.'
                                                |> String.fromChar
                                    in
                                    String.toInt str == Nothing && str /= "."
                            in
                            case ( digit, state ) of
                                ( Nothing, ParsingNumber hasSymbol value ) ->
                                    if
                                        hasSymbol
                                            || isSymbolIn colIndex previousRow
                                            || isSymbolIn colIndex currentRow
                                            || isSymbolIn colIndex nextRow
                                    then
                                        ( iacc + value, Not )

                                    else
                                        ( iacc, Not )

                                ( Just d, ParsingNumber hasSymbol value ) ->
                                    ( iacc
                                    , ParsingNumber
                                        (hasSymbol
                                            || isSymbolIn colIndex previousRow
                                            || isSymbolIn colIndex nextRow
                                        )
                                        (value * 10 + d)
                                    )

                                ( Just d, Not ) ->
                                    ( iacc
                                    , ParsingNumber
                                        (isSymbolIn (colIndex - 1) previousRow
                                            || isSymbolIn (colIndex - 1) currentRow
                                            || isSymbolIn (colIndex - 1) nextRow
                                            || isSymbolIn colIndex previousRow
                                            || isSymbolIn colIndex nextRow
                                        )
                                        d
                                    )

                                ( Nothing, Not ) ->
                                    ( iacc, Not )
                        )
                        ( acc, Not )
                    |> Tuple.first
            )
            0
        |> String.fromInt
        |> Ok


type ScanState
    = ParsingNumber Bool Int
    | Not


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
