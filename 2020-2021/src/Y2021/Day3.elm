module Y2021.Day3 exposing (parser, process, processGold)

import List.Extra
import Parser exposing (Parser)


type alias Item =
    List Int


parser : Parser Item
parser =
    Parser.map
        (\line ->
            line
                |> String.toList
                |> List.filterMap (String.fromChar >> String.toInt)
        )
        Parser.getSource


process : List Item -> Int
process lines =
    let
        ( gamma, epsilon ) =
            lines
                |> List.Extra.transpose
                |> List.map (\l -> ( getCommon Most l, getCommon Least l ))
                |> List.unzip
    in
    listToBinary gamma * listToBinary epsilon


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
        o2 =
            listToBinary <| goldHelper Most lines

        co2 =
            listToBinary <| goldHelper Least lines
    in
    o2 * co2


goldHelper : Common -> List Item -> Item
goldHelper common left =
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
                    goldHelper common
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
