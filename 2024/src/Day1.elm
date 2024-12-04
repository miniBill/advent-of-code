module Day1 exposing (run)

import BackendTask exposing (BackendTask)
import Dict exposing (Dict)
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
    Utils.runLineBased
        { day = 1
        , examples = [ ( example, 11, 31 ) ]
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


part2 : List ( Int, Int ) -> Int
part2 lines =
    let
        ( lefts, rights ) =
            List.unzip lines

        counts : Dict Int Int
        counts =
            Dict.Extra.frequencies rights
    in
    lefts
        |> List.map (\left -> left * (Dict.get left counts |> Maybe.withDefault 0))
        |> List.sum


part1 : List ( Int, Int ) -> Int
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
