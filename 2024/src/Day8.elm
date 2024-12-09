module Day8 exposing (run)

import BackendTask exposing (BackendTask)
import Dict
import Dict.Extra
import FatalError exposing (FatalError)
import Grid exposing (Grid)
import Pages.Script as Script exposing (Script)
import Parser exposing (Parser)
import Set
import Triple.Extra
import Utils


example : String
example =
    """............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............"""


run : Script
run =
    Script.withoutCliOptions task


task : BackendTask FatalError ()
task =
    Utils.run
        { day = 8
        , examples =
            [ ( example, 14, 34 )
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
    grid
        |> Grid.toIndexedCellsList
        |> Dict.Extra.groupBy Triple.Extra.third
        |> Dict.remove '.'
        |> Dict.values
        |> List.concatMap
            (\antennas ->
                antennas
                    |> List.concatMap
                        (\( fr, fc, _ ) ->
                            antennas
                                |> List.filterMap
                                    (\( sr, sc, _ ) ->
                                        if fr == sr && fc == sc then
                                            Nothing

                                        else
                                            let
                                                r : Int
                                                r =
                                                    2 * sr - fr

                                                c : Int
                                                c =
                                                    2 * sc - fc
                                            in
                                            if Grid.inRange r c grid then
                                                Just ( r, c )

                                            else
                                                Nothing
                                    )
                        )
            )
        |> Set.fromList
        |> Set.size


part2 : Grid -> Int
part2 grid =
    let
        q : Int
        q =
            max (Grid.rows grid) (Grid.columns grid)
    in
    grid
        |> Grid.toIndexedCellsList
        |> Dict.Extra.groupBy Triple.Extra.third
        |> Dict.remove '.'
        |> Dict.values
        |> List.concatMap
            (\antennas ->
                antennas
                    |> List.concatMap
                        (\( fr, fc, _ ) ->
                            antennas
                                |> List.concatMap
                                    (\( sr, sc, _ ) ->
                                        -- We could do some smart math here.
                                        -- Ooooor, we just bruteforce it
                                        List.range -q q
                                            |> List.filterMap
                                                (\k ->
                                                    if fr == sr && fc == sc then
                                                        Nothing

                                                    else
                                                        let
                                                            r : Int
                                                            r =
                                                                fr + k * (sr - fr)

                                                            c : Int
                                                            c =
                                                                fc + k * (sc - fc)
                                                        in
                                                        if Grid.inRange r c grid then
                                                            Just ( r, c )

                                                        else
                                                            Nothing
                                                )
                                    )
                        )
            )
        |> Set.fromList
        |> Set.size
