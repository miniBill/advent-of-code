module Day2 exposing (run)

import BackendTask exposing (BackendTask)
import FatalError exposing (FatalError)
import List.Extra
import Pages.Script as Script exposing (Script)
import Parser exposing (Parser)
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
    Utils.runLineBased
        { day = 2
        , examples = [ ( example, 2, 4 ) ]
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
        , spaces = Parser.chompWhile (\c -> c == ' ')
        , item = Parser.int
        , trailing = Parser.Optional
        }


part1 : List (List Int) -> Int
part1 lines =
    List.Extra.count isSafe lines


part2 : List (List Int) -> Int
part2 lines =
    List.Extra.count isDampenedSafe lines


isDampenedSafe : List Int -> Bool
isDampenedSafe levels =
    -- isSafe levels ||
    List.any
        (\index -> isSafe (List.Extra.removeAt index levels))
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
                direction : Order
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
        diff : Int
        diff =
            abs (l - r)
    in
    1 <= diff && diff <= 3
