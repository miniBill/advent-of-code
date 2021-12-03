module Y2021.Day3 exposing (parser, process, processGold)

import List.Extra
import Parser exposing ((|.), (|=), Parser, Trailing(..))


type alias Item =
    List Int


parser : Parser Item
parser =
    Parser.getSource
        |> Parser.map
            (\line ->
                line
                    |> String.toList
                    |> List.filterMap (String.fromChar >> String.toInt)
            )


process : List Item -> Int
process lines =
    let
        go f =
            lines
                |> List.Extra.transpose
                |> List.filterMap
                    (\l ->
                        l
                            |> List.Extra.gatherEquals
                            |> List.sortBy (Tuple.second >> List.length >> f)
                            |> List.head
                            |> Maybe.map Tuple.first
                    )
                |> listToBinary

        gamma =
            go negate

        epsilon =
            go identity
    in
    gamma * epsilon


listToBinary : List Int -> Int
listToBinary =
    List.foldl (\e a -> a * 2 + e) 0


processGold : List Item -> Int
processGold lines =
    0
