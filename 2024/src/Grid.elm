module Grid exposing (Grid, columns, count, find, fromLists, get, getUnsafe, rows, set)

import Array exposing (Array)


type Grid a
    = Grid { rows : Int, columns : Int, cells : Array a }


fromLists : List (List a) -> Grid a
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


get : Int -> Int -> Grid a -> Maybe a
get r c (Grid grid) =
    if c < 0 || r < 0 || c >= grid.columns || r >= grid.rows then
        Nothing

    else
        Array.get (r * grid.columns + c) grid.cells


getUnsafe : Int -> Int -> Grid a -> a
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


find : a -> Grid a -> Maybe ( Int, Int )
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


set : Int -> Int -> a -> Grid a -> Grid a
set r c value (Grid grid) =
    if c < 0 || r < 0 || c >= grid.columns || r >= grid.rows then
        Grid grid

    else
        Grid
            { rows = grid.rows
            , columns = grid.columns
            , cells = Array.set (r * grid.columns + c) value grid.cells
            }


count : (a -> Bool) -> Grid a -> Int
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


rows : Grid a -> Int
rows (Grid grid) =
    grid.rows


columns : Grid a -> Int
columns (Grid grid) =
    grid.columns
