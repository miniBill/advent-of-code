module Day4 exposing (run)

import BackendTask exposing (BackendTask)
import Dict
import FatalError exposing (FatalError)
import List.Extra
import Pages.Script as Script exposing (Script)
import Parser exposing (Parser)
import Parser.Workaround
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


run : Script
run =
    Script.withoutCliOptions task


task : BackendTask FatalError ()
task =
    Utils.runLineBased
        { day = 4
        , examples =
            [ ( smol, 4, -1 )
            , ( large, 18, -1 )
            , ( sparse, 18, -1 )
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
    -1
