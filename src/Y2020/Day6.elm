module Y2020.Day6 exposing (parser, process, processGold)

import List.Extra
import Parser exposing ((|.), (|=), Parser)
import Set exposing (Set)


type alias Item =
    Set Char


parser : Parser Item
parser =
    Parser.getSource
        |> Parser.map (String.toList >> Set.fromList)


aggregate : List Item -> List ( Item, List Item )
aggregate list =
    list
        |> List.Extra.groupWhile (\a b -> Set.isEmpty a == Set.isEmpty b)
        |> List.Extra.filterNot (\( h, _ ) -> Set.isEmpty h)


process : List Item -> Int
process list =
    list
        |> aggregate
        |> List.map
            (\( h, t ) ->
                List.foldr Set.union h t
                    |> Set.size
            )
        |> List.sum


processGold : List Item -> Int
processGold list =
    list
        |> aggregate
        |> List.map
            (\( h, t ) ->
                List.foldr Set.intersect h t
                    |> Set.size
            )
        |> List.sum
