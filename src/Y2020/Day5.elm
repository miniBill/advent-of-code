module Y2020.Day5 exposing (parser, process, processGold)

import Parser exposing ((|.), (|=), Parser)
import Parser.Extras


type alias Item =
    Int


parser : Parser Item
parser =
    let
        item =
            Parser.getChompedString (Parser.chompIf (always True))
    in
    Parser.Extras.many item
        |> Parser.map
            (List.foldl
                (\e a ->
                    a
                        * 2
                        + (if e == "B" || e == "R" then
                            1

                           else
                            0
                          )
                )
                0
            )


process : List Item -> Int
process =
    List.maximum >> Maybe.withDefault -1


processGold : List Item -> Int
processGold list =
    list
        |> List.sort
        |> List.foldl
            (\e ( last, found ) ->
                if e == last + 1 then
                    ( e, found )

                else
                    ( e, e - 1 )
            )
            ( 0, 0 )
        |> Tuple.second
