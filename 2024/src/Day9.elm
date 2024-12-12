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
            , ( long, 1928, 2858 )
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


type alias File =
    { index : Int
    , id : Id
    , length : Int
    }


part1 : Item -> Int
part1 item =
    let
        expanded : { files : List ( Int, Id ), free : Set Int }
        expanded =
            expand1 item

        go : List ( Int, Id ) -> List Int -> List ( Int, Id ) -> Int
        go queue free acc =
            case queue of
                [] ->
                    finalSum acc

                (( index, digit ) as head) :: tail ->
                    case free of
                        [] ->
                            finalSum (head :: tail ++ acc)

                        minFree :: newFree ->
                            if minFree < index then
                                go tail newFree (( minFree, digit ) :: acc)

                            else
                                finalSum (head :: tail ++ acc)
    in
    go expanded.files (Set.toList expanded.free) []


finalSum : List ( Int, Id ) -> Int
finalSum acc =
    acc
        |> List.map (\( index, Id id ) -> id * index)
        |> List.sum


expand1 : Item -> { files : List ( Int, Id ), free : Set Int }
expand1 input =
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
                , { files = List.foldl (\i acc -> ( i, Id (round id) ) :: acc) files range
                  , free = free
                  }
                )

            else
                ( nextAcc
                , { files = files
                  , free = Set.union (Set.fromList range) free
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
    let
        expanded : { files : List File, free : Set ( Int, Int ) }
        expanded =
            expand2 item

        go : List File -> List ( Int, Int ) -> List ( Int, Id ) -> Int
        go queue free acc =
            -- let
            --     _ =
            --         Debug.log "Step" (viewStep (List.concatMap fileToBlocks queue ++ acc))
            --     _ =
            --         Debug.log "free" free
            -- in
            case queue of
                [] ->
                    finalSum acc

                ({ index, id, length } as head) :: tail ->
                    case findFreeBlockOfLength length free of
                        Nothing ->
                            go tail free (fileToBlocks head ++ acc)

                        Just ( minFree, newFree ) ->
                            if minFree < index then
                                go tail
                                    newFree
                                    (fileToBlocks
                                        { index = minFree
                                        , id = id
                                        , length = length
                                        }
                                        ++ acc
                                    )

                            else
                                go tail free (fileToBlocks head ++ acc)
    in
    go expanded.files (Set.toList expanded.free) []



-- viewStep : List ( Int, Id ) -> String
-- viewStep blocks =
--     let
--         dict : Dict Int Id
--         dict =
--             Dict.fromList blocks
--     in
--     List.range 0 (Dict.getMaxKey dict |> Maybe.withDefault 0)
--         |> List.map
--             (\i ->
--                 case Dict.get i dict of
--                     Just (Id id) ->
--                         String.fromInt id
--                     Nothing ->
--                         "."
--             )
--         |> String.concat


findFreeBlockOfLength : Int -> List ( Int, Int ) -> Maybe ( Int, List ( Int, Int ) )
findFreeBlockOfLength length free =
    let
        go : List ( Int, Int ) -> List ( Int, Int ) -> Maybe ( Int, List ( Int, Int ) )
        go queue acc =
            case queue of
                [] ->
                    Nothing

                (( freeIndex, freeLength ) as head) :: tail ->
                    if freeLength >= length then
                        Just
                            ( freeIndex
                            , if freeLength == length then
                                List.reverse acc ++ tail

                              else
                                List.reverse acc ++ ( freeIndex + length, freeLength - length ) :: tail
                            )

                    else
                        go tail (head :: acc)
    in
    go free []


fileToBlocks : File -> List ( Int, Id )
fileToBlocks { index, id, length } =
    List.map (\i -> ( i, id )) (List.range index (index + length - 1))


expand2 : Item -> { files : List File, free : Set ( Int, Int ) }
expand2 input =
    List.foldl
        (\length ( { id, isFile, index }, { files, free } ) ->
            let
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
                        { index = index
                        , id = Id (round id)
                        , length = length
                        }
                            :: files
                  , free = free
                  }
                )

            else
                ( nextAcc
                , { files = files
                  , free =
                        if length > 0 then
                            Set.insert ( index, length ) free

                        else
                            free
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
