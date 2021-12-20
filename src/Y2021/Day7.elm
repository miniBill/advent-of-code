module Y2021.Day7 exposing (parser, process, processGold)

import Parser exposing (Parser)


type alias Item =
    List Int


parser : Parser Item
parser =
    Parser.sequence
        { start = ""
        , end = ""
        , item = Parser.int
        , separator = ","
        , spaces = Parser.succeed ()
        , trailing = Parser.Forbidden
        }



--|> Parser.map (List.foldl (\e -> Dict.update e (Maybe.withDefault 0 >> (+) 1 >> Just)) Dict.empty)


process : Item -> String
process line =
    let
        len =
            List.length line

        count =
            2 - modBy 2 len

        mid =
            line
                |> List.sort
                |> List.drop ((len - 1) // 2)
                |> List.take count
                |> List.sum
                |> (\s -> s // count)
    in
    line
        |> List.map (\e -> abs (e - mid))
        |> List.sum
        |> String.fromInt


processGold : Item -> String
processGold line =
    let
        cost mid =
            List.map
                (\e ->
                    let
                        delta =
                            e - mid
                    in
                    delta * delta + abs delta
                )
                line
                |> List.sum
                |> (\n -> n // 2)

        ( mn, mx ) =
            case line of
                [] ->
                    ( 0, 0 )

                h :: t ->
                    List.foldl (\e ( n, x ) -> ( min n e, max x e )) ( h, h ) t
    in
    List.range mn mx
        |> List.foldl
            (\mid (( bestCost, _ ) as best) ->
                let
                    c =
                        cost mid
                in
                if c < bestCost then
                    ( c, mid )

                else
                    best
            )
            ( cost mn, mn )
        |> (\( c, m ) -> String.fromInt m ++ ": " ++ String.fromInt c)
