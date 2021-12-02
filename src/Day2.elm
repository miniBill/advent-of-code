module Day2 exposing (parser, process, processGold)

import Parser exposing ((|.), (|=), Parser)


type Command
    = Forward Int
    | Down Int
    | Up Int


parser : Parser Command
parser =
    Parser.succeed identity
        |= Parser.oneOf
            [ Parser.succeed Forward
                |. Parser.keyword "forward"
            , Parser.succeed Down
                |. Parser.keyword "down"
            , Parser.succeed Up
                |. Parser.keyword "up"
            ]
        |. Parser.spaces
        |= Parser.int


process : List Command -> Int
process lines =
    lines
        |> List.foldl
            (\command ( x, depth ) ->
                case command of
                    Forward dx ->
                        ( x + dx, depth )

                    Down dd ->
                        ( x, depth + dd )

                    Up dd ->
                        ( x, depth - dd )
            )
            ( 0, 0 )
        |> (\( x, depth ) -> x * depth)


processGold : List Command -> Int
processGold lines =
    lines
        |> List.foldl
            (\command ( x, aim, depth ) ->
                case command of
                    Forward dx ->
                        ( x + dx, aim, depth + aim * dx )

                    Down dd ->
                        ( x, aim + dd, depth )

                    Up dd ->
                        ( x, aim - dd, depth )
            )
            ( 0, 0, 0 )
        |> (\( x, _, depth ) -> x * depth)
