module Day1 exposing (process, processGold)


parse : List String -> List Int
parse lines =
    List.filterMap String.toInt lines


process : List String -> String
process lines =
    case parse lines of
        [] ->
            "Invalid input"

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
                |> String.fromInt


processGold : List String -> String
processGold lines =
    case parse lines of
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
                |> String.fromInt

        _ ->
            "Invalid input"
