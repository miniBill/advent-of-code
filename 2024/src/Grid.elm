module Grid exposing (GenericGrid, Grid, columns, count, find, fromLists, get, getUnsafe, inRange, parser, rows, set, toIndexedCellsList)

import Array exposing (Array)
import Parser exposing (Parser)
import Triple.Extra


type alias Grid =
    GenericGrid Char


type GenericGrid a
    = Grid
        { rows : Int
        , columns : Int
        , cells : Array a
        }


fromLists : List (List a) -> GenericGrid a
fromLists lists =
    Grid
        { rows = List.length lists
        , columns =
            lists
                |> List.head
                |> Maybe.map List.length
                |> Maybe.withDefault 0
        , cells = lists |> List.concat |> Array.fromList
        }


get : Int -> Int -> GenericGrid a -> Maybe a
get r c (Grid grid) =
    if inRange c r (Grid grid) then
        Array.get (r * grid.columns + c) grid.cells

    else
        Nothing


inRange : Int -> Int -> GenericGrid a -> Bool
inRange c r (Grid grid) =
    c >= 0 && r >= 0 && c < grid.columns && r < grid.rows


getUnsafe : Int -> Int -> GenericGrid a -> a
getUnsafe r c grid =
    case get r c grid of
        Just res ->
            res

        Nothing ->
            Debug.todo
                ("getUnsafe "
                    ++ String.fromInt r
                    ++ " "
                    ++ String.fromInt c
                )


find : a -> GenericGrid a -> Maybe ( Int, Int )
find value (Grid grid) =
    Array.foldl
        (\cell acc ->
            case acc of
                Err i ->
                    if cell == value then
                        Ok ( i // grid.columns, modBy grid.columns i )

                    else
                        Err (i + 1)

                Ok _ ->
                    acc
        )
        (Err 0)
        grid.cells
        |> Result.toMaybe


set : Int -> Int -> a -> GenericGrid a -> GenericGrid a
set r c value (Grid grid) =
    if inRange r c (Grid grid) then
        Grid
            { rows = grid.rows
            , columns = grid.columns
            , cells = Array.set (r * grid.columns + c) value grid.cells
            }

    else
        Grid grid


count : (a -> Bool) -> GenericGrid a -> Int
count f (Grid grid) =
    Array.foldl
        (\cell acc ->
            if f cell then
                acc + 1

            else
                acc
        )
        0
        grid.cells


rows : GenericGrid a -> Int
rows (Grid grid) =
    grid.rows


columns : GenericGrid a -> Int
columns (Grid grid) =
    grid.columns


parser : Parser Grid
parser =
    Parser.chompWhile (\_ -> True)
        |> Parser.getChompedString
        |> Parser.map
            (\raw ->
                raw
                    |> String.trim
                    |> String.split "\n"
                    |> List.map String.toList
                    |> fromLists
            )


toIndexedCellsList : GenericGrid a -> List ( Int, Int, a )
toIndexedCellsList (Grid grid) =
    Array.foldl
        (\cell ( rowIndex, colIndex, acc ) ->
            let
                newAcc =
                    ( rowIndex, colIndex, cell ) :: acc
            in
            if colIndex == grid.columns - 1 then
                ( rowIndex + 1, 0, newAcc )

            else
                ( rowIndex, colIndex + 1, newAcc )
        )
        ( 0, 0, [] )
        grid.cells
        |> Triple.Extra.third
