module Y2021.Day1 exposing (parser, process, processGold)

import Parser exposing (Parser)


parser : Parser Int
parser =
    Parser.int


process : List Int -> Int
process lines =
    case lines of
        h :: t ->
            t
                |> List.foldl
                    (\e ( count, last ) ->
                        ( if e > last then
                            count + 1

                          else
                            count
                        , e
                        )
                    )
                    ( 0, h )
                |> Tuple.first

        [] ->
            0


processGold : List Int -> Int
processGold lines =
    case lines of
        ha :: hb :: hc :: t ->
            t
                |> List.foldl
                    (\e ( count, ( a, b, c ) ) ->
                        ( if b + c + e > a + b + c then
                            count + 1

                          else
                            count
                        , ( b, c, e )
                        )
                    )
                    ( 0, ( ha, hb, hc ) )
                |> Tuple.first

        _ ->
            0
