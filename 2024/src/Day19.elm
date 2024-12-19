module Day19 exposing (run)

import BackendTask exposing (BackendTask)
import Dict exposing (Dict)
import FatalError exposing (FatalError)
import List.Extra
import Pages.Script as Script exposing (Script)
import Parser exposing ((|.), (|=), Parser)
import Set exposing (Set)
import Utils


example1 : String
example1 =
    """r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb"""


run : Script
run =
    Script.withoutCliOptions task


task : BackendTask FatalError ()
task =
    Utils.run
        { day = 19

        -- , examples = [ ( example1, 6, 16 ) ]
        , examples = [ ( example1, -1, 16 ) ]
        , parser = parser
        , solver1 = part1
        , solver2 = part2
        }


type alias Input =
    { patterns : Set String
    , targets : List String
    }


parser : Parser Input
parser =
    Parser.succeed Input
        |= (Parser.sequence
                { start = ""
                , end = ""
                , separator = ", "
                , spaces = Parser.succeed ()
                , trailing = Parser.Forbidden
                , item = patternParser
                }
                |> Parser.map Set.fromList
           )
        |. Parser.spaces
        |= Parser.sequence
            { start = ""
            , end = ""
            , separator = "\n"
            , spaces = Parser.succeed ()
            , trailing = Parser.Optional
            , item = patternParser
            }


patternParser : Parser String
patternParser =
    Parser.getChompedString (Parser.chompWhile (\c -> c == 'r' || c == 'g' || c == 'u' || c == 'b' || c == 'w'))


part1 : Input -> Int
part1 input =
    List.foldl
        (\line ( dacc, cacc ) ->
            let
                ( newDacc, count ) =
                    isDoableWith input.patterns line dacc
            in
            ( newDacc
            , if count > 0 then
                cacc + 1

              else
                cacc
            )
        )
        ( Dict.empty, 0 )
        input.targets
        |> Tuple.second


part2 : Input -> Int
part2 input =
    List.foldl
        (\line ( dacc, cacc ) ->
            let
                ( newDacc, count ) =
                    isDoableWith input.patterns line dacc
            in
            ( newDacc
            , cacc + count
            )
        )
        ( Dict.empty, 0 )
        input.targets
        |> Tuple.second


isDoableWith : Set String -> String -> Dict String Int -> ( Dict String Int, Int )
isDoableWith patterns current found =
    case Dict.get current found of
        Just r ->
            ( found, r )

        Nothing ->
            let
                ( finalFound, result ) =
                    List.foldl
                        (\sliceSize ( dacc, cacc ) ->
                            let
                                before : String
                                before =
                                    String.left sliceSize current
                            in
                            if not (Set.member before patterns) then
                                ( dacc, cacc )

                            else
                                let
                                    after : String
                                    after =
                                        String.right (String.length current - sliceSize) current

                                    ( finalInnerFound, countAfter ) =
                                        isDoableWith patterns after dacc
                                in
                                ( finalInnerFound, cacc + countAfter )
                        )
                        ( found
                        , if Set.member current patterns then
                            1

                          else
                            0
                        )
                        (List.range 1 (String.length current - 1))
            in
            ( Dict.insert current result finalFound, result )
