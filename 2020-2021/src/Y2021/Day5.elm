module Y2021.Day5 exposing (parser, process, processGold)

import Dict exposing (Dict)
import List.Extra
import Parser exposing ((|.), (|=), Parser)


type alias Item =
    ( Point, Point )


type alias Point =
    ( Int, Int )


parser : Parser Item
parser =
    Parser.succeed Tuple.pair
        |= pointParser
        |. Parser.symbol " -> "
        |= pointParser


pointParser : Parser Point
pointParser =
    Parser.succeed Tuple.pair
        |= Parser.int
        |. Parser.symbol ","
        |= Parser.int


process : List Item -> String
process lines =
    lines
        |> List.foldl
            (\( ( fx, fy ) as from, ( tx, ty ) as to ) acc ->
                if (fx == tx) || (fy == ty) then
                    generate from to
                        |> List.foldl setCell acc

                else
                    acc
            )
            Dict.empty
        |> Dict.values
        |> List.Extra.count (\c -> c >= 2)
        |> String.fromInt


generate : Point -> Point -> List Point
generate (( fx, fy ) as from) (( tx, ty ) as to) =
    if fx > tx then
        generate to from

    else if fx == tx then
        List.range (min fy ty) (max fy ty) |> List.map (\y -> Tuple.pair fx y)

    else
        let
            dy =
                sign <| ty - fy
        in
        List.range fx tx
            |> List.map (\x -> ( x, fy + (x - fx) * dy ))


sign : Int -> Int
sign n =
    if n > 0 then
        1

    else if n == 0 then
        0

    else
        -1


setCell : Point -> Dict Point Int -> Dict Point Int
setCell p =
    Dict.update p (Maybe.withDefault 0 >> (+) 1 >> Just)


processGold : List Item -> String
processGold lines =
    lines
        |> List.foldl
            (\( from, to ) acc ->
                generate from to
                    |> List.foldl setCell acc
            )
            Dict.empty
        |> Dict.values
        |> List.Extra.count (\c -> c >= 2)
        |> String.fromInt
