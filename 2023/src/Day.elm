module Day exposing (Day(..), fromInt, toInt)


type Day
    = Day1
    | Day2
    | Day3
    | Day4


toInt : Day -> Int
toInt day =
    case day of
        Day1 ->
            1

        Day2 ->
            2

        Day3 ->
            3

        Day4 ->
            4


fromInt : Int -> Maybe Day
fromInt day =
    case day of
        1 ->
            Just Day1

        2 ->
            Just Day2

        3 ->
            Just Day3

        4 ->
            Just Day4

        _ ->
            Nothing
