module Day1 exposing (process, processGold)


process : List String -> String
process lines =
    List.length lines |> String.fromInt


processGold : List String -> String
processGold lines =
    List.length lines |> String.fromInt
