module Day9 exposing (run)

import BackendTask exposing (BackendTask)
import FastSet as Set exposing (Set)
import FatalError exposing (FatalError)
import Pages.Script as Script exposing (Script)
import Parser exposing (Parser)
import Utils


short : String
short =
    "12345"


long : String
long =
    "2333133121414131402"


run : Script
run =
    Script.withoutCliOptions task


task : BackendTask FatalError ()
task =
    Utils.run
        { day = 9
        , examples =
            [ ( short, 60, -1 )
            , ( long, 1928, -1 )
            ]
        , parser = parser
        , solver1 = part1
        , solver2 = part2
        }


type Id
    = Id Int


type alias Item =
    List Int


parser : Parser Item
parser =
    Parser.sequence
        { start = ""
        , end = ""
        , spaces = Parser.succeed ()
        , separator = ""
        , item =
            Parser.chompIf (\_ -> True)
                |> Parser.getChompedString
                |> Parser.andThen
                    (\c ->
                        case String.toInt c of
                            Just i ->
                                Parser.succeed i

                            Nothing ->
                                Parser.problem ("Invalid digit: '" ++ c ++ "'")
                    )
        , trailing = Parser.Forbidden
        }


part1 : Item -> Int
part1 item =
    let
        expanded : { files : List ( Int, Id ), free : Set Int }
        expanded =
            expand item

        go : List ( Int, Id ) -> Set Int -> List ( Int, Id ) -> Int
        go queue free acc =
            case queue of
                [] ->
                    acc
                        |> List.map (\( index, Id id ) -> id * index)
                        |> List.sum

                (( index, digit ) as head) :: tail ->
                    case Set.popMin free of
                        Nothing ->
                            go [] free (head :: tail ++ acc)

                        Just ( minFree, newFree ) ->
                            if minFree < index then
                                go tail newFree (( minFree, digit ) :: acc)

                            else
                                go [] newFree (head :: tail ++ acc)
    in
    go expanded.files expanded.free []


expand : Item -> { files : List ( Int, Id ), free : Set Int }
expand input =
    List.foldl
        (\length ( { id, isFile, index }, { files, free } ) ->
            let
                range : List Int
                range =
                    List.range index (index + length - 1)

                nextAcc : { id : Float, isFile : Bool, index : Int }
                nextAcc =
                    { id = id + 0.5
                    , isFile = not isFile
                    , index = index + length
                    }
            in
            if isFile then
                ( nextAcc
                , { files =
                        List.foldl
                            (\i acc -> ( i, Id (round id) ) :: acc)
                            files
                            range
                  , free = free
                  }
                )

            else
                ( nextAcc
                , { files = files
                  , free =
                        List.foldl
                            (\i acc -> Set.insert i acc)
                            free
                            range
                  }
                )
        )
        ( { id = 0
          , isFile = True
          , index = 0
          }
        , { files = [], free = Set.empty }
        )
        input
        |> Tuple.second


part2 : Item -> Int
part2 item =
    0
