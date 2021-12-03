module Y2020.Day3 exposing (parser, process, processGold)

import Parser exposing ((|.), (|=), Parser)


type alias Item =
    String


parser : Parser Item
parser =
    Parser.getSource


process : List Item -> Int
process =
    processSlope ( 3, 1 )


processGold : List Item -> Int
processGold list =
    [ ( 1, 1 ), ( 3, 1 ), ( 5, 1 ), ( 7, 1 ), ( 1, 2 ) ]
        |> List.map (\slope -> processSlope slope list)
        |> List.product


processSlope : ( Int, Int ) -> List String -> Int
processSlope ( right, down ) list =
    list
        |> List.foldl
            (\line ( index, pos, trees ) ->
                if modBy down index == 0 then
                    ( index + 1
                    , modBy (String.length line) (pos + right)
                    , if String.slice pos (pos + 1) line == "#" then
                        trees + 1

                      else
                        trees
                    )

                else
                    ( index + 1, pos, trees )
            )
            ( 0, 0, 0 )
        |> (\( _, _, trees ) -> trees)
