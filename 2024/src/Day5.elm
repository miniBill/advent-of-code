module Day5 exposing (run)

import BackendTask exposing (BackendTask)
import FatalError exposing (FatalError)
import List.Extra
import Pages.Script as Script exposing (Script)
import Parser exposing ((|.), (|=), Parser)
import Set exposing (Set)
import Utils


example : String
example =
    """47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47"""


run : Script
run =
    Script.withoutCliOptions task


task : BackendTask FatalError ()
task =
    Utils.runLineBased
        { day = 5
        , examples =
            [ ( example, 143, 123 )
            ]
        , parser = parser
        , solver1 = part1
        , solver2 = part2
        }


type Line
    = Empty
    | Before Int Int
    | Update (List Int)


parser : Parser Line
parser =
    Parser.oneOf
        [ Parser.succeed Before
            |= Parser.backtrackable Parser.int
            |. Parser.symbol "|"
            |= Parser.int
        , Parser.succeed (\head tail -> Update (head :: tail))
            |= Parser.int
            |= Parser.oneOf
                [ Parser.succeed identity
                    |. Parser.symbol ","
                    |= Parser.sequence
                        { start = ""
                        , end = ""
                        , separator = ","
                        , spaces = Parser.succeed ()
                        , item = Parser.int
                        , trailing = Parser.Forbidden
                        }
                , Parser.succeed []
                ]
        , Parser.succeed Empty
        ]


split : List Line -> ( List ( Int, Int ), List (List Int) )
split lines =
    let
        ( befores, updates ) =
            List.foldl
                (\line ( bacc, uacc ) ->
                    case line of
                        Empty ->
                            ( bacc, uacc )

                        Before l r ->
                            ( ( l, r ) :: bacc, uacc )

                        Update ns ->
                            ( bacc, ns :: uacc )
                )
                ( [], [] )
                lines
    in
    ( List.reverse befores, List.reverse updates )


part1 : List Line -> Int
part1 lines =
    let
        ( befores, updates ) =
            split lines

        bounds : Set ( Int, Int )
        bounds =
            Set.fromList befores

        ifCorrectMiddle : List Int -> Maybe Int
        ifCorrectMiddle line =
            if isCorrect bounds line then
                Just (middle line)

            else
                Nothing
    in
    List.filterMap ifCorrectMiddle updates
        |> List.sum


isCorrect : Set ( Int, Int ) -> List Int -> Bool
isCorrect bounds line =
    case line of
        [] ->
            True

        head :: tail ->
            if List.all (\other -> not (Set.member ( other, head ) bounds)) tail then
                isCorrect bounds tail

            else
                False


middle : List Int -> Int
middle line =
    line
        |> List.drop (List.length line // 2)
        |> List.head
        |> Maybe.withDefault -9999


part2 : List Line -> Int
part2 lines =
    let
        ( befores, updates ) =
            split lines

        bounds : Set ( Int, Int )
        bounds =
            Set.fromList befores

        ifIncorrectFixThenMiddle : List Int -> Maybe Int
        ifIncorrectFixThenMiddle line =
            if isCorrect bounds line then
                Nothing

            else
                Just (middle (fix bounds line))
    in
    List.filterMap ifIncorrectFixThenMiddle updates
        |> List.sum


fix : Set ( Int, Int ) -> List Int -> List Int
fix bounds line =
    let
        go : List Int -> List Int -> List Int
        go queue acc =
            case queue of
                [] ->
                    List.reverse acc

                head :: tail ->
                    case List.Extra.find (\other -> Set.member ( other, head ) bounds) tail of
                        Nothing ->
                            go tail (head :: acc)

                        Just oth ->
                            go (oth :: head :: List.Extra.remove oth tail) acc
    in
    go line []
