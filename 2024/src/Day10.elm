module Day10 exposing (run)

import BackendTask exposing (BackendTask)
import FatalError exposing (FatalError)
import Graph exposing (Graph)
import Graph.Tree as Tree
import Grid exposing (GenericGrid)
import Pages.Script as Script exposing (Script)
import Parser exposing (Parser)
import Utils


example1 : String
example1 =
    """0123
1234
8765
9876"""


example2 : String
example2 =
    """89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732"""


run : Script
run =
    Script.withoutCliOptions task


task : BackendTask FatalError ()
task =
    Utils.run
        { day = 10
        , examples =
            [ ( example1, 1, -1 )
            , ( example2, 36, -1 )
            ]
        , parser = parser
        , solver1 = part1
        , solver2 = part2
        }


parser : Parser (GenericGrid Int)
parser =
    Grid.parser
        |> Parser.map
            (Grid.map
                (\c ->
                    c
                        |> String.fromChar
                        |> String.toInt
                        |> Maybe.withDefault -1
                )
            )


part1 : GenericGrid Int -> Int
part1 grid =
    let
        w : Int
        w =
            Grid.columns grid

        graph : Graph Int ()
        graph =
            Grid.foldul
                (\r c h acc ->
                    let
                        tryLink :
                            Int
                            -> Int
                            -> List ( Graph.NodeId, Graph.NodeId )
                            -> List ( Graph.NodeId, Graph.NodeId )
                        tryLink r2 c2 d =
                            case Grid.get r2 c2 grid of
                                Nothing ->
                                    d

                                Just h2 ->
                                    if h2 == h + 1 then
                                        ( r * w + c, r2 * w + c2 ) :: d

                                    else
                                        d
                    in
                    acc
                        |> tryLink (r - 1) c
                        |> tryLink (r + 1) c
                        |> tryLink r (c - 1)
                        |> tryLink r (c + 1)
                )
                []
                grid
                |> Graph.fromNodeLabelsAndEdgePairs (Grid.foldbr (\_ _ cell acc -> cell :: acc) [] grid)
    in
    Grid.findAll 0 grid
        |> List.map
            (\( r, c ) ->
                graph
                    |> Graph.dfsTree (r * w + c)
                    |> Tree.preOrder
                        (\context _ acc ->
                            if context.node.label == 9 then
                                acc + 1

                            else
                                acc
                        )
                        0
            )
        |> List.sum


part2 : GenericGrid Int -> Int
part2 _ =
    0
