module Day7 exposing (run)

import BackendTask exposing (BackendTask)
import FatalError exposing (FatalError)
import Pages.Script as Script exposing (Script)
import Parser exposing ((|.), (|=), Parser)
import Utils


example : String
example =
    """190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20"""


run : Script
run =
    Script.withoutCliOptions task


task : BackendTask FatalError ()
task =
    Utils.runLineBased
        { day = 7
        , examples =
            [ ( example, 3749, 11387 )
            ]
        , parser = parser
        , solver1 = part1
        , solver2 = part2
        }


type alias Item =
    ( Int, List Int )


parser : Parser Item
parser =
    Parser.succeed Tuple.pair
        |= Parser.int
        |. Parser.symbol ": "
        |= Parser.sequence
            { start = ""
            , end = ""
            , spaces = Parser.succeed ()
            , separator = " "
            , item = Parser.int
            , trailing = Parser.Forbidden
            }


part1 : List Item -> Int
part1 lines =
    List.foldl
        (\( target, inputs ) acc ->
            if hasSolution [ (*), (+) ] target inputs then
                acc + target

            else
                acc
        )
        0
        lines


hasSolution : List (Int -> Int -> Int) -> Int -> List Int -> Bool
hasSolution ops target input =
    let
        canSkip : Bool
        canSkip =
            List.all (\i -> i > 0) input

        go : Int -> List Int -> Bool
        go acc queue =
            if canSkip && acc > target then
                False

            else
                case queue of
                    [] ->
                        acc == target

                    head :: tail ->
                        List.any
                            (\op -> go (op acc head) tail)
                            ops
    in
    case input of
        [] ->
            False

        head :: tail ->
            go head tail


part2 : List Item -> Int
part2 lines =
    List.foldl
        (\( target, inputs ) acc ->
            if hasSolution [ concatNumbers, (*), (+) ] target inputs then
                acc + target

            else
                acc
        )
        0
        lines


concatNumbers : Int -> Int -> Int
concatNumbers l r =
    String.toInt (String.fromInt l ++ String.fromInt r)
        |> Maybe.withDefault -9999999
