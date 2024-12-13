module Day11 exposing (run)

import BackendTask exposing (BackendTask)
import FastDict as Dict exposing (Dict)
import FatalError exposing (FatalError)
import Pages.Script as Script exposing (Script)
import Parser exposing (Parser)
import Utils


example1 : String
example1 =
    """125 17"""


run : Script
run =
    Script.withoutCliOptions task


task : BackendTask FatalError ()
task =
    Utils.run
        { day = 11
        , examples =
            [ ( example1, 55312, -1 )
            ]
        , parser = parser
        , solver1 = part1
        , solver2 = part2
        }


parser : Parser (List Int)
parser =
    Parser.sequence
        { start = ""
        , end = ""
        , separator = " "
        , spaces = Parser.succeed ()
        , item = Parser.int
        , trailing = Parser.Forbidden
        }


part1 : List Int -> Int
part1 list =
    go 25 list Dict.empty
        |> Tuple.second


part2 : List Int -> Int
part2 list =
    go 75 list Dict.empty
        |> Tuple.second


go : Int -> List Int -> Dict ( Int, Int ) Int -> ( Dict ( Int, Int ) Int, Int )
go blinks queue known =
    if blinks == 0 then
        ( known, List.length queue )

    else
        List.foldl
            (\num ( dacc, sacc ) ->
                let
                    next : List Int
                    next =
                        if num == 0 then
                            [ 1 ]

                        else
                            let
                                numLength : Int
                                numLength =
                                    String.length (String.fromInt num)
                            in
                            if modBy 2 numLength == 0 then
                                let
                                    halfPower : Int
                                    halfPower =
                                        10 ^ (numLength // 2)

                                    l : Int
                                    l =
                                        num // halfPower

                                    r : Int
                                    r =
                                        num |> modBy halfPower
                                in
                                [ l, r ]

                            else
                                [ num * 2024 ]
                in
                List.foldl
                    (\nextNum ( idacc, isacc ) ->
                        case Dict.get ( blinks - 1, nextNum ) idacc of
                            Just res ->
                                ( known, res + isacc )

                            Nothing ->
                                let
                                    ( nextiDacc, res ) =
                                        go (blinks - 1) [ nextNum ] idacc
                                in
                                ( Dict.insert ( blinks - 1, nextNum ) res nextiDacc, res + isacc )
                    )
                    ( dacc, sacc )
                    next
            )
            ( known, 0 )
            queue
