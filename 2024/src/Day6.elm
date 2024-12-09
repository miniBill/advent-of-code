module Day6 exposing (run)

import BackendTask exposing (BackendTask)
import FatalError exposing (FatalError)
import Grid exposing (Grid)
import List.Extra
import Pages.Script as Script exposing (Script)
import Parser exposing (Parser)
import SeqSet exposing (SeqSet)
import Set exposing (Set)
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
    Utils.run
        { day = 6
        , examples =
            [ ( example, 41, 6 )
            ]
        , parser = parser
        , solver1 = part1
        , solver2 = part2
        }


parser : Parser Grid
parser =
    Grid.parser


part1 : Grid -> Int
part1 grid =
    let
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
            part1Grid direction r c grid Set.empty
                |> Set.size


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


part1Grid : Direction -> Int -> Int -> Grid -> Set ( Int, Int ) -> Set ( Int, Int )
part1Grid direction r c grid seen =
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

        nextSeen : Set ( Int, Int )
        nextSeen =
            Set.insert ( r, c ) seen
    in
    case Grid.get nextR nextC grid of
        Nothing ->
            nextSeen

        Just '#' ->
            part1Grid (rotate direction) r c grid nextSeen

        Just _ ->
            part1Grid direction nextR nextC grid nextSeen


part2 : Grid -> Int
part2 grid =
    let
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
            part1Grid direction r c grid Set.empty
                |> Set.toList
                |> List.Extra.count
                    (\( obsR, obsC ) ->
                        part2Grid direction r c obsR obsC SeqSet.empty grid
                    )


part2Grid : Direction -> Int -> Int -> Int -> Int -> SeqSet ( Direction, Int, Int ) -> Grid -> Bool
part2Grid direction r c obsR obsC seen grid =
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

            nextSeen : SeqSet ( Direction, Int, Int )
            nextSeen =
                SeqSet.insert ( direction, r, c ) seen
        in
        if nextR == obsR && nextC == obsC then
            part2Grid (rotate direction) r c obsR obsC nextSeen grid

        else
            case Grid.get nextR nextC grid of
                Nothing ->
                    False

                Just '#' ->
                    part2Grid (rotate direction) r c obsR obsC nextSeen grid

                Just _ ->
                    part2Grid direction nextR nextC obsR obsC nextSeen grid
