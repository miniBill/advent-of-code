module Day3 exposing (run)

import BackendTask exposing (BackendTask)
import FatalError exposing (FatalError)
import Pages.Script as Script exposing (Script)
import Parser exposing ((|.), (|=), Parser)
import Utils


example : String
example =
    """xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"""


run : Script
run =
    Script.withoutCliOptions task


task : BackendTask FatalError ()
task =
    Utils.run
        { day = 3
        , example = example
        , parser = parser
        , exampleSolution1 = 161
        , exampleSolution2 = 48
        , solver1 = part1
        , solver2 = part2
        }


type Instruction
    = Mul Int Int
    | Do
    | Dont


parser : Parser (List Instruction)
parser =
    Parser.sequence
        { start = ""
        , end = ""
        , separator = ""
        , spaces = Parser.spaces
        , item =
            Parser.oneOf
                [ Parser.backtrackable
                    (Parser.succeed (\a b -> Just (Mul a b))
                        |. Parser.symbol "mul("
                        |= Parser.int
                        |. Parser.symbol ","
                        |= Parser.int
                        |. Parser.symbol ")"
                    )
                , Parser.succeed (Just Do)
                    |. Parser.symbol "do()"
                , Parser.succeed (Just Dont)
                    |. Parser.symbol "don't()"
                , Parser.succeed Nothing
                    |. Parser.chompIf (\_ -> True)
                ]
        , trailing = Parser.Optional
        }
        |. Parser.end
        |> Parser.map (List.filterMap identity)


part1 : List Instruction -> Int
part1 lines =
    lines
        |> List.filterMap
            (\instruction ->
                case instruction of
                    Mul a b ->
                        Just (a * b)

                    _ ->
                        Nothing
            )
        |> List.sum


part2 : List Instruction -> Int
part2 lines =
    lines
        |> List.foldl
            (\instruction ( acc, enabled ) ->
                case instruction of
                    Mul a b ->
                        if enabled then
                            ( acc + a * b, enabled )

                        else
                            ( acc, enabled )

                    Do ->
                        ( acc, True )

                    Dont ->
                        ( acc, False )
            )
            ( 0, True )
        |> Tuple.first
