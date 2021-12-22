module Y2021.Day9 exposing (parser, process, processGold)

import Dict
import List.Extra
import Parser exposing ((|.), (|=), Parser)
import UnionFind


type alias Item =
    List Int


parser : Parser Item
parser =
    Parser.succeed identity
        |. Parser.chompIf Char.isDigit
        |. Parser.chompWhile Char.isDigit
        |> Parser.getChompedString
        |> Parser.map (String.toList >> List.filterMap (String.fromChar >> String.toInt))


process : List Item -> { output : String, log : List String }
process lines =
    let
        unrolled =
            unroll lines
    in
    { output =
        unrolled
            |> List.map
                (List.map
                    (\cell ->
                        if isMinimum cell then
                            cell.cell + 1

                        else
                            0
                    )
                    >> List.sum
                )
            |> List.sum
            |> String.fromInt
    , log =
        [ "Parsed as\n" ++ gridToString String.fromInt lines
        , "Unrolled as\n"
            ++ gridToString
                (\cell ->
                    if isMinimum cell then
                        "X"

                    else
                        String.fromInt cell.cell
                )
                unrolled
        ]
    }


isMinimum : { cell : Int, top : Maybe Int, left : Maybe Int, right : Maybe Int, bottom : Maybe Int } -> Bool
isMinimum { cell, top, left, right, bottom } =
    let
        lt x =
            case x of
                Nothing ->
                    True

                Just v ->
                    cell < v
    in
    lt top && lt bottom && lt left && lt right


gridToString : (x -> String) -> List (List x) -> String
gridToString f lines =
    "  " ++ String.join "\n  " (List.map (String.concat << List.map f) lines)


unroll : List Item -> List (List CellWithNeighbours)
unroll lines =
    let
        cols =
            List.head lines |> Maybe.withDefault [] |> List.length

        emptyRow =
            List.repeat (cols + 2) Nothing

        ( _, _, result ) =
            lines
                |> List.map (\row -> Nothing :: List.map Just row ++ [ Nothing ])
                |> (\l -> emptyRow :: l ++ [ emptyRow ])
                |> List.foldl
                    (\nextRow ( previousRow, currentRow, accRows ) ->
                        ( currentRow
                        , nextRow
                        , if List.isEmpty previousRow || List.isEmpty currentRow then
                            accRows

                          else
                            let
                                ( _, _, folded ) =
                                    List.foldl
                                        (\(( _, nextCell, _ ) as nextTrio) ( previousCell, ( currentTop, currentCell, currentBottom ), accCells ) ->
                                            ( currentCell
                                            , nextTrio
                                            , case currentCell of
                                                Nothing ->
                                                    accCells

                                                Just c ->
                                                    { cell = c
                                                    , top = currentTop
                                                    , bottom = currentBottom
                                                    , left = previousCell
                                                    , right = nextCell
                                                    }
                                                        :: accCells
                                            )
                                        )
                                        ( Nothing, ( Nothing, Nothing, Nothing ), [] )
                                        (List.map3 (\a b c -> ( a, b, c )) previousRow currentRow nextRow)
                            in
                            List.reverse folded :: accRows
                        )
                    )
                    ( [], [], [] )
    in
    List.reverse result


type alias CellWithNeighbours =
    { cell : Int
    , top : Maybe Int
    , left : Maybe Int
    , right : Maybe Int
    , bottom : Maybe Int
    }


processGold : List Item -> { output : String, log : List String }
processGold lines =
    let
        unrolled =
            unroll lines

        indexed =
            List.indexedMap
                (\rowIndex ->
                    List.indexedMap
                        (\colIndex cell ->
                            ( rowIndex, colIndex, cell )
                        )
                )
                unrolled

        united =
            List.foldl
                (\row acc -> List.foldl unionStep acc row)
                UnionFind.quickUnionPathCompression
                indexed

        unionStep ( rowIndex, colIndex, cell ) acc =
            if cell.cell == 9 then
                acc

            else
                let
                    mergeIfNotPlateau neighbourIndex neighbourValue =
                        case neighbourValue of
                            Nothing ->
                                identity

                            Just 9 ->
                                identity

                            Just _ ->
                                UnionFind.union ( rowIndex, colIndex ) neighbourIndex
                in
                acc
                    |> mergeIfNotPlateau ( rowIndex, colIndex + 1 ) cell.right
                    |> mergeIfNotPlateau ( rowIndex + 1, colIndex ) cell.bottom

        gathered =
            indexed
                |> List.concatMap
                    (List.filterMap
                        (\( rowIndex, colIndex, cell ) ->
                            let
                                pos =
                                    ( rowIndex, colIndex )
                            in
                            if cell.cell == 9 then
                                Nothing

                            else
                                Just ( UnionFind.find pos united, pos )
                        )
                    )
                |> List.Extra.gatherEqualsBy Tuple.first
                |> List.indexedMap (\i ( ( _, h ), t ) -> ( i, h :: List.map Tuple.second t ))

        coordToIsland =
            gathered
                |> List.foldl
                    (\( i, poss ) acc ->
                        List.foldl (\pos -> Dict.insert pos i) acc poss
                    )
                    Dict.empty

        biggestIslands =
            gathered
                |> List.map (Tuple.second >> List.length)
                |> List.sortBy negate
                |> List.take 3
                |> List.product
    in
    { output =
        biggestIslands
            |> String.fromInt
    , log =
        [ "Parsed as\n" ++ gridToString String.fromInt lines
        , "Islands are\n"
            ++ gridToString
                (\( rowIndex, colIndex, cell ) ->
                    if cell.cell == 9 then
                        " "

                    else
                        case Dict.get ( rowIndex, colIndex ) coordToIsland of
                            Just i ->
                                String.fromChar (Char.fromCode (i + Char.toCode '!'))

                            Nothing ->
                                String.fromInt cell.cell
                )
                indexed
        ]
    }
