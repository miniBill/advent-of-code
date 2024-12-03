module Utils exposing (testThenRun)

import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import BackendTask.Http as Http
import FatalError exposing (FatalError)
import Json.Encode
import List.Extra
import Pages.Script as Script
import Parser exposing ((|.), Parser)
import Result.Extra


testThenRun :
    { day : Int
    , example : String
    , exampleSolution1 : String
    , exampleSolution2 : String
    , parser : Parser item
    , solver1 : List item -> BackendTask FatalError String
    , solver2 : List item -> BackendTask FatalError String
    }
    -> BackendTask FatalError ()
testThenRun { day, example, exampleSolution1, exampleSolution2, parser, solver1, solver2 } =
    let
        parse : List String -> BackendTask FatalError (List item)
        parse input =
            input
                |> List.indexedMap
                    (\index line ->
                        Parser.run (parser |. Parser.end) line
                            |> Result.mapError
                                (\_ ->
                                    let
                                        msg : String
                                        msg =
                                            "Failed to parse line "
                                                ++ String.fromInt (index + 1)
                                                ++ ": "
                                                ++ escape line
                                    in
                                    FatalError.fromString msg
                                )
                    )
                |> Result.Extra.combine
                |> BackendTask.fromResult
    in
    Do.do (parse (toLines example)) <| \parsedExample ->
    Do.do (solver1 parsedExample) <| \exampleActual1 ->
    if exampleActual1 /= exampleSolution1 then
        BackendTask.fail (FatalError.fromString ("(part1) Expected example solution to be " ++ escape exampleSolution1 ++ " but got " ++ escape exampleActual1))

    else
        Do.do (solver2 parsedExample) <| \exampleActual2 ->
        if exampleActual2 /= exampleSolution2 then
            BackendTask.fail (FatalError.fromString ("(part2) Expected example solution to be " ++ escape exampleSolution2 ++ " but got " ++ escape exampleActual2))

        else
            withInputLinesForDay day <| \input ->
            Do.do (parse input) <| \parsedInput ->
            Do.do (solver1 parsedInput) <| \solution1 ->
            Do.do (solver2 parsedInput) <| \solution2 ->
            Do.log ("Solution (part 1): " ++ solution1) <| \_ ->
            Script.log ("Solution (part 2): " ++ solution2)


escape : String -> String
escape line =
    Json.Encode.encode 0 (Json.Encode.string line)


withInputLinesForDay : Int -> (List String -> BackendTask FatalError r) -> BackendTask FatalError r
withInputLinesForDay day f =
    withInputForDay day <| \input ->
    f (toLines input)


toLines : String -> List String
toLines input =
    input
        |> String.split "\n"
        |> List.Extra.dropWhileRight String.isEmpty


withInputForDay : Int -> (String -> BackendTask FatalError r) -> BackendTask FatalError r
withInputForDay day f =
    Do.env "SESSION_COOKIE" <| \sessionCookie ->
    Do.allowFatal
        (Http.getWithOptions
            { url = "https://adventofcode.com/2024/day/" ++ String.fromInt day ++ "/input"
            , expect = Http.expectString
            , cachePath = Nothing
            , cacheStrategy = Nothing
            , headers = [ ( "Cookie", "session=" ++ sessionCookie ) ]
            , retries = Nothing
            , timeoutInMs = Nothing
            }
        )
        f
