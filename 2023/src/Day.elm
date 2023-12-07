module Day exposing (Day(..), fromInt, toInt)


type Day
    = Day1
    | Day2
    | Day3
    | Day4
    | Day5
    | Day6
    | Day7
    | Day8
    | Day9
    | Day10
    | Day11
    | Day12
    | Day13
    | Day14
    | Day15
    | Day16
    | Day17
    | Day18
    | Day19
    | Day20
    | Day21
    | Day22
    | Day23
    | Day24
    | Day25


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

        Day5 ->
            5

        Day6 ->
            6

        Day7 ->
            7

        Day8 ->
            8

        Day9 ->
            9

        Day10 ->
            10

        Day11 ->
            11

        Day12 ->
            12

        Day13 ->
            13

        Day14 ->
            14

        Day15 ->
            15

        Day16 ->
            16

        Day17 ->
            17

        Day18 ->
            18

        Day19 ->
            19

        Day20 ->
            20

        Day21 ->
            21

        Day22 ->
            22

        Day23 ->
            23

        Day24 ->
            24

        Day25 ->
            25


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

        5 ->
            Just Day5

        6 ->
            Just Day6

        7 ->
            Just Day7

        8 ->
            Just Day8

        9 ->
            Just Day9

        10 ->
            Just Day10

        11 ->
            Just Day11

        12 ->
            Just Day12

        13 ->
            Just Day13

        14 ->
            Just Day14

        15 ->
            Just Day15

        16 ->
            Just Day16

        17 ->
            Just Day17

        18 ->
            Just Day18

        19 ->
            Just Day19

        20 ->
            Just Day20

        21 ->
            Just Day21

        22 ->
            Just Day22

        23 ->
            Just Day23

        24 ->
            Just Day24

        25 ->
            Just Day25

        _ ->
            Nothing
