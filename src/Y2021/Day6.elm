module Y2021.Day6 exposing (parser, process, processGold)

import Dict exposing (Dict)
import Parser exposing (Parser)


type alias Item =
    Dict Int Int


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
        |> Parser.map (List.foldl (\e -> Dict.update e (Maybe.withDefault 0 >> (+) 1 >> Just)) Dict.empty)


process : Item -> String
process =
    iterate 80


iterate : Int -> Dict Int Int -> String
iterate days status =
    if days <= 0 then
        String.fromInt <| List.sum <| Dict.values status

    else
        let
            get k =
                Dict.get k status |> Maybe.withDefault 0

            babies =
                get 0
        in
        List.range 0 8
            |> List.map
                (\day ->
                    ( day
                    , case day of
                        6 ->
                            get 7 + babies

                        8 ->
                            babies

                        _ ->
                            get (day + 1)
                    )
                )
            |> Dict.fromList
            |> iterate (days - 1)


processGold : Item -> String
processGold =
    iterate 256
