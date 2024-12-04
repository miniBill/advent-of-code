module Day4 exposing (run)

import BackendTask exposing (BackendTask)
import Dict
import FatalError exposing (FatalError)
import List.Extra
import Pages.Script as Script exposing (Script)
import Parser exposing (Parser)
import Parser.Workaround
import Triple.Extra
import Utils


smol : String
smol =
    """..X...
.SAMX.
.A..A.
XMAS.S
.X...."""


large : String
large =
    """MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX"""


sparse : String
sparse =
    """....XXMAS.
.SAMXMS...
...S..A...
..A.A.MS.X
XMASAMX.MM
X.....XA.A
S.S.S.S.SS
.A.A.A.A.A
..M.M.M.MM
.X.X.XMASX
"""


sparse2 : String
sparse2 =
    """.M.S......
..A..MSMS.
.M.S.MAA..
..A.ASMSM.
.M.S.M....
..........
S.S.S.S.S.
.A.A.A.A..
M.M.M.M.M.
.........."""


run : Script
run =
    Script.withoutCliOptions task


task : BackendTask FatalError ()
task =
    Utils.runLineBased
        { day = 4
        , examples =
            [ ( smol, 4, 0 )
            , ( large, 18, 9 )
            , ( sparse, 18, 3 )
            , ( sparse2, 0, 9 )
            ]
        , parser = parser
        , solver1 = part1
        , solver2 = part2
        }


parser : Parser (List Char)
parser =
    Parser.Workaround.chompUntilEndOrBefore "\n"
        |> Parser.getChompedString
        |> Parser.map String.toList


part1 : List (List Char) -> Int
part1 lines =
    let
        -- _ =
        --     Debug.log
        --         ("Top corner:\n  "
        --             ++ (lines
        --                     |> List.take 5
        --                     |> List.map
        --                         (\line ->
        --                             line
        --                                 |> List.take 5
        --                                 |> String.fromList
        --                         )
        --                     |> String.join "\n  "
        --                )
        --         )
        --         ()
        -- _ =
        --     Debug.log "Partials"
        --         { horizontal = horizontal
        --         , vertical = vertical
        --         , diagonalMain = diagonalMain
        --         , diagonalReverse = diagonalReverse
        --         }
        -- _ =
        --     Debug.log "Diagonals"
        --         { diagonalPlus = diagonals (+)
        --         , diagonalMinus = diagonals (-)
        --         }
        findAndReverse : List (List Char) -> Int
        findAndReverse input =
            (input
                |> List.map findXmas
                |> List.sum
            )
                + (input
                    |> List.map
                        (\line ->
                            line
                                |> List.reverse
                                |> findXmas
                        )
                    |> List.sum
                  )

        horizontal : Int
        horizontal =
            findAndReverse lines

        vertical : Int
        vertical =
            findAndReverse (List.Extra.transpose lines)

        diagonals : (Int -> Int -> Int) -> List (List Char)
        diagonals f =
            List.foldl
                (\row ( rowIndex, oacc ) ->
                    ( rowIndex + 1
                    , List.foldl
                        (\cell ( colIndex, iacc ) ->
                            let
                                key : Int
                                key =
                                    f rowIndex colIndex
                            in
                            ( colIndex + 1
                            , Dict.insert key
                                (Dict.get key iacc
                                    |> Maybe.withDefault []
                                    |> (::) cell
                                )
                                iacc
                            )
                        )
                        ( 0, oacc )
                        row
                        |> Tuple.second
                    )
                )
                ( 0, Dict.empty )
                lines
                |> Tuple.second
                |> Dict.values

        diagonalMain : Int
        diagonalMain =
            findAndReverse (diagonals (-))

        diagonalReverse : Int
        diagonalReverse =
            findAndReverse (diagonals (+))
    in
    horizontal + vertical + diagonalMain + diagonalReverse


findXmas : List Char -> Int
findXmas input =
    let
        go : List Char -> Int -> Int
        go queue acc =
            case queue of
                [] ->
                    acc

                'X' :: 'M' :: 'A' :: 'S' :: tail ->
                    go tail (acc + 1)

                _ :: tail ->
                    go tail acc
    in
    go input 0


part2 : List (List Char) -> Int
part2 lines =
    case lines of
        first :: second :: tail ->
            List.foldl
                (\line ( prev1, prev2, acc ) ->
                    let
                        go : List Char -> List Char -> List Char -> Int -> Int
                        go a b c innerAcc =
                            case ( a, b, c ) of
                                ( 'M' :: _ :: 'S' :: _, _ :: 'A' :: _ :: _, 'M' :: _ :: 'S' :: _ ) ->
                                    go (List.drop 1 a) (List.drop 1 b) (List.drop 1 c) (innerAcc + 1)

                                ( 'M' :: _ :: 'M' :: _, _ :: 'A' :: _ :: _, 'S' :: _ :: 'S' :: _ ) ->
                                    go (List.drop 1 a) (List.drop 1 b) (List.drop 1 c) (innerAcc + 1)

                                ( 'S' :: _ :: 'M' :: _, _ :: 'A' :: _ :: _, 'S' :: _ :: 'M' :: _ ) ->
                                    go (List.drop 1 a) (List.drop 1 b) (List.drop 1 c) (innerAcc + 1)

                                ( 'S' :: _ :: 'S' :: _, _ :: 'A' :: _ :: _, 'M' :: _ :: 'M' :: _ ) ->
                                    go (List.drop 1 a) (List.drop 1 b) (List.drop 1 c) (innerAcc + 1)

                                ( _ :: ((_ :: _ :: _ :: _) as tailA), _ :: ((_ :: _ :: _ :: _) as tailB), _ :: ((_ :: _ :: _ :: _) as tailC) ) ->
                                    go tailA tailB tailC innerAcc

                                _ ->
                                    innerAcc
                    in
                    ( line, prev1, go line prev1 prev2 acc )
                )
                ( second, first, 0 )
                tail
                |> Triple.Extra.third

        _ ->
            0
