module Day2 exposing (run)

import BackendTask exposing (BackendTask)
import Dict
import Dict.Extra
import FatalError exposing (FatalError)
import List.Extra
import Pages.Script as Script exposing (Script)
import Parser exposing ((|.), (|=), Parser)
import Utils


example : String
example =
    """7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9"""


run : Script
run =
    Script.withoutCliOptions task


task : BackendTask FatalError ()
task =
    Utils.testThenRunWithParserBoth
        { day = 2
        , example = example
        , exampleSolution1 = "2"
        , exampleSolution2 = "4"
        , parser = parser
        , solver1 = part1
        , solver2 = part2
        }


parser : Parser (List Int)
parser =
    Parser.sequence
        { start = ""
        , end = ""
        , separator = ""
        , spaces = Parser.spaces
        , item = Parser.int
        , trailing = Parser.Optional
        }


part2 : List (List Int) -> BackendTask error String
part2 lines =
    lines
        |> List.Extra.count isDampenedSafe
        |> String.fromInt
        |> BackendTask.succeed


part1 : List (List Int) -> BackendTask FatalError String
part1 lines =
    lines
        |> List.Extra.count isSafe
        |> String.fromInt
        |> BackendTask.succeed


isDampenedSafe : List Int -> Bool
isDampenedSafe levels =
    isSafe levels
        || List.any (\index -> isSafe (List.Extra.removeAt index levels))
            (List.range 0 (List.length levels - 1))


isSafe : List Int -> Bool
isSafe levels =
    case levels of
        [] ->
            True

        [ _ ] ->
            True

        [ first, second ] ->
            safeDiff first second

        first :: second :: tail ->
            let
                direction =
                    compare first second
            in
            List.foldl
                (\e ( safe, last ) ->
                    ( safe && compare last e == direction && safeDiff last e
                    , e
                    )
                )
                ( True, first )
                (second :: tail)
                |> Tuple.first


safeDiff : Int -> Int -> Bool
safeDiff l r =
    let
        diff =
            abs (l - r)
    in
    1 <= diff && diff <= 3
