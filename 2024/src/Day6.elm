module Day6 exposing (run)

import Array
import BackendTask exposing (BackendTask)
import Dict
import FatalError exposing (FatalError)
import Grid exposing (Grid)
import List.Extra
import Maybe.Extra
import Pages.Script as Script exposing (Script)
import Parser exposing (Parser)
import Parser.Workaround
import SeqSet exposing (SeqSet)
import Triple.Extra
import Utils


example : String
example =
    """....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...
"""


run : Script
run =
    Script.withoutCliOptions task


task : BackendTask FatalError ()
task =
    Utils.runLineBased
        { day = 6
        , examples =
            [ ( example, 41, 6 )
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
        grid : Grid Char
        grid =
            Grid.fromLists lines

        found : Maybe ( Direction, ( Int, Int ) )
        found =
            [ Up, Down, Right, Left ]
                |> List.Extra.findMap
                    (\direction ->
                        Grid.find (directionToChar direction) grid
                            |> Maybe.map (\pos -> ( direction, pos ))
                    )
    in
    case found of
        Nothing ->
            0

        Just ( direction, ( r, c ) ) ->
            part1Grid direction r c grid


type Direction
    = Up
    | Right
    | Down
    | Left


directionToChar : Direction -> Char
directionToChar direction =
    case direction of
        Up ->
            '^'

        Down ->
            'V'

        Right ->
            '>'

        Left ->
            '<'


rotate : Direction -> Direction
rotate direction =
    case direction of
        Up ->
            Right

        Right ->
            Down

        Down ->
            Left

        Left ->
            Up


part1Grid : Direction -> Int -> Int -> Grid Char -> Int
part1Grid direction r c grid =
    let
        ( nextR, nextC ) =
            case direction of
                Up ->
                    ( r - 1, c )

                Down ->
                    ( r + 1, c )

                Right ->
                    ( r, c + 1 )

                Left ->
                    ( r, c - 1 )

        done : Grid Char -> Int
        done finalGrid =
            Grid.count ((==) 'X') finalGrid

        nextGrid : Grid Char
        nextGrid =
            Grid.set r c 'X' grid
    in
    case Grid.get nextR nextC grid of
        Nothing ->
            done nextGrid

        Just '#' ->
            part1Grid (rotate direction) r c nextGrid

        Just _ ->
            part1Grid direction nextR nextC nextGrid


part2 : List (List Char) -> Int
part2 lines =
    let
        grid : Grid Char
        grid =
            Grid.fromLists lines

        found : Maybe ( Direction, ( Int, Int ) )
        found =
            [ Up, Down, Right, Left ]
                |> List.Extra.findMap
                    (\direction ->
                        Grid.find (directionToChar direction) grid
                            |> Maybe.map (\pos -> ( direction, pos ))
                    )
    in
    case found of
        Nothing ->
            0

        Just ( direction, ( r, c ) ) ->
            let
                gridPoints : List ( Int, Int )
                gridPoints =
                    List.Extra.lift2 Tuple.pair
                        (List.range 0 (Grid.rows grid))
                        (List.range 0 (Grid.columns grid))
            in
            List.Extra.count
                (\( obsR, obsC ) ->
                    let
                        _ =
                            if obsC == 0 then
                                Debug.log
                                    ("Running "
                                        ++ String.fromInt obsR
                                        ++ "/"
                                        ++ String.fromInt (Grid.rows grid)
                                    )
                                    ()

                            else
                                ()
                    in
                    part2Grid direction r c SeqSet.empty (Grid.set obsR obsC '#' grid)
                )
                gridPoints


part2Grid : Direction -> Int -> Int -> SeqSet ( Direction, Int, Int ) -> Grid Char -> Bool
part2Grid direction r c seen grid =
    if SeqSet.member ( direction, r, c ) seen then
        True

    else
        let
            ( nextR, nextC ) =
                case direction of
                    Up ->
                        ( r - 1, c )

                    Down ->
                        ( r + 1, c )

                    Right ->
                        ( r, c + 1 )

                    Left ->
                        ( r, c - 1 )
        in
        case Grid.get nextR nextC grid of
            Nothing ->
                False

            Just '#' ->
                part2Grid (rotate direction) r c (SeqSet.insert ( direction, r, c ) seen) grid

            Just _ ->
                part2Grid direction nextR nextC (SeqSet.insert ( direction, r, c ) seen) grid
