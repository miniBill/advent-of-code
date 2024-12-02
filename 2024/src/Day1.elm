module Day1 exposing (run)

import BackendTask exposing (BackendTask)
import Dict
import Dict.Extra
import FatalError exposing (FatalError)
import Pages.Script as Script exposing (Script)
import Parser exposing ((|.), (|=), Parser)
import Utils


example : String
example =
    """3   4
4   3
2   5
1   3
3   9
3   3"""


run : Script
run =
    Script.withoutCliOptions task


task : BackendTask FatalError ()
task =
    Utils.testThenRunWithParserBoth
        { day = 1
        , example = example
        , exampleSolution1 = "11"
        , exampleSolution2 = "31"
        , parser = parser
        , solver1 = part1
        , solver2 = part2
        }


parser : Parser ( Int, Int )
parser =
    Parser.succeed Tuple.pair
        |= Parser.int
        |. Parser.spaces
        |= Parser.int


part2 : List ( Int, Int ) -> BackendTask error String
part2 lines =
    let
        ( lefts, rights ) =
            List.unzip lines

        counts =
            Dict.Extra.frequencies rights
    in
    lefts
        |> List.map (\left -> left * (Dict.get left counts |> Maybe.withDefault 0))
        |> List.sum
        |> String.fromInt
        |> BackendTask.succeed


part1 : List ( Int, Int ) -> BackendTask FatalError String
part1 lines =
    let
        ( lefts, rights ) =
            List.unzip lines
    in
    List.map2
        (\l r -> abs (l - r))
        (List.sort lefts)
        (List.sort rights)
        |> List.sum
        |> String.fromInt
        |> BackendTask.succeed
