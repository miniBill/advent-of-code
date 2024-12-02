module Utils exposing (..)

import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import BackendTask.Http as Http
import FatalError exposing (FatalError)
import Json.Encode
import List.Extra
import Pages.Script as Script
import Parser exposing (Parser)
import Result.Extra


testThenRunWithParser :
    { day : Int
    , example : String
    , exampleSolution : String
    , parser : Parser item
    , solver : List item -> BackendTask FatalError String
    }
    -> BackendTask FatalError ()
testThenRunWithParser { day, example, exampleSolution, parser, solver } =
    testThenRun
        { day = day
        , example = example
        , exampleSolution = exampleSolution
        , solver = \lines -> combineParserAndSolver parser solver lines
        }


testThenRunWithParserBoth :
    { day : Int
    , example : String
    , exampleSolution1 : String
    , exampleSolution2 : String
    , parser : Parser item
    , solver1 : List item -> BackendTask FatalError String
    , solver2 : List item -> BackendTask FatalError String
    }
    -> BackendTask FatalError ()
testThenRunWithParserBoth { day, example, exampleSolution1, exampleSolution2, parser, solver1, solver2 } =
    let
        combineSolutions l r =
            l ++ " and " ++ r
    in
    testThenRun
        { day = day
        , example = example
        , exampleSolution = combineSolutions exampleSolution1 exampleSolution2
        , solver =
            \lines ->
                combineParserAndSolver parser
                    (\parsed ->
                        BackendTask.map2 combineSolutions
                            (solver1 parsed)
                            (solver2 parsed)
                    )
                    lines
        }


combineParserAndSolver :
    Parser item
    -> (List item -> BackendTask FatalError String)
    -> List String
    -> BackendTask FatalError String
combineParserAndSolver parser solver lines =
    lines
        |> List.indexedMap
            (\index line ->
                Parser.run parser line
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
        |> BackendTask.andThen solver


escape : String -> String
escape line =
    Json.Encode.encode 0 (Json.Encode.string line)


testThenRun :
    { day : Int
    , example : String
    , exampleSolution : String
    , solver : List String -> BackendTask FatalError String
    }
    -> BackendTask FatalError ()
testThenRun { day, example, exampleSolution, solver } =
    Do.do (solver (toLines example)) <| \exampleActual ->
    if exampleActual /= exampleSolution then
        BackendTask.fail (FatalError.fromString ("Expected example solution to be " ++ escape exampleSolution ++ " but got " ++ escape exampleActual))

    else
        withInputLinesForDay day <| \input ->
        Do.do (solver input) <| \solution ->
        Script.log ("Solution: " ++ solution)


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
