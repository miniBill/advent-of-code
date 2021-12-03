module Y2021.Day3 exposing (parser, process, processGold)

import List.Extra
import Parser exposing (Parser)


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
        transposed =
            lines
                |> List.Extra.transpose

        go common =
            transposed
                |> List.map (getCommon common)
                |> listToBinary

        gamma =
            go Most

        epsilon =
            go Least
    in
    gamma * epsilon


type Common
    = Most
    | Least


getCommon : Common -> List Int -> Int
getCommon common list =
    case List.sortBy Tuple.first <| List.Extra.gatherEquals list of
        [ ( 0, zc ), ( 1, oc ) ] ->
            case common of
                Most ->
                    if List.length oc >= List.length zc then
                        1

                    else
                        0

                Least ->
                    if List.length zc <= List.length oc then
                        0

                    else
                        1

        [ ( 0, _ ) ] ->
            0

        [ ( 1, _ ) ] ->
            1

        _ ->
            -- The list is either empty or contains something that is not one nor zero
            -1


listToBinary : List Int -> Int
listToBinary =
    List.foldl (\e a -> a * 2 + e) 0


processGold : List Item -> Int
processGold lines =
    let
        go common left =
            case left of
                [] :: _ ->
                    []

                [ x ] ->
                    x

                _ ->
                    let
                        unpacked =
                            List.filterMap List.Extra.uncons left

                        chosen =
                            unpacked
                                |> List.map Tuple.first
                                |> getCommon common

                        tail =
                            go common
                                (List.filterMap
                                    (\( first, rest ) ->
                                        if first == chosen then
                                            Just rest

                                        else
                                            Nothing
                                    )
                                    unpacked
                                )
                    in
                    chosen :: tail
    in
    listToBinary (go Most lines) * listToBinary (go Least lines)
