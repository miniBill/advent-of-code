module Utils exposing (run, runLineBased)

import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import BackendTask.File as File
import BackendTask.Http as Http
import BackendTask.Time
import FatalError exposing (FatalError)
import Pages.Script as Script
import Parser exposing ((|.), Parser)
import Time


runLineBased :
    { day : Int
    , examples : List ( String, Int, Int )
    , parser : Parser item
    , solver1 : List item -> Int
    , solver2 : List item -> Int
    }
    -> BackendTask FatalError ()
runLineBased { day, examples, parser, solver1, solver2 } =
    run
        { day = day
        , examples = examples
        , parser =
            Parser.sequence
                { start = ""
                , end = ""
                , item = parser
                , separator = "\n"
                , spaces = Parser.succeed ()
                , trailing = Parser.Optional
                }
                |. Parser.end
        , solver1 = solver1
        , solver2 = solver2
        }


run :
    { day : Int
    , examples : List ( String, Int, Int )
    , parser : Parser input
    , solver1 : input -> Int
    , solver2 : input -> Int
    }
    -> BackendTask FatalError ()
run { day, examples, parser, solver1, solver2 } =
    let
        parse : String -> BackendTask FatalError input
        parse input =
            Parser.run (parser |. Parser.end) (String.trim input)
                |> Result.mapError
                    (\e ->
                        let
                            msg : String
                            msg =
                                "Failed to parse input: " ++ Debug.toString e
                        in
                        FatalError.fromString msg
                    )
                |> BackendTask.fromResult

        timeDiff : Time.Posix -> Time.Posix -> String
        timeDiff from to =
            String.fromInt (Time.posixToMillis to - Time.posixToMillis from) ++ "ms"

        path : String
        path =
            "input/day" ++ String.fromInt day ++ ".txt"
    in
    Do.each examples
        (\( input, out1, out2 ) ->
            parse input
                |> BackendTask.map (\parsed -> ( parsed, out1, out2 ))
        )
    <| \parsedExamples ->
    Do.each parsedExamples
        (\( parsedExample, exampleSolution1, exampleSolution2 ) ->
            let
                exampleActual1 : Int
                exampleActual1 =
                    solver1 parsedExample
            in
            if exampleActual1 /= exampleSolution1 then
                BackendTask.fail (FatalError.fromString ("(part1) Expected example solution to be " ++ String.fromInt exampleSolution1 ++ " but got " ++ String.fromInt exampleActual1))

            else
                let
                    exampleActual2 : Int
                    exampleActual2 =
                        solver2 parsedExample
                in
                if exampleSolution2 /= -1 && exampleActual2 /= exampleSolution2 then
                    BackendTask.fail (FatalError.fromString ("(part2) Expected example solution to be " ++ String.fromInt exampleSolution2 ++ " but got " ++ String.fromInt exampleActual2))

                else
                    BackendTask.succeed ()
        )
    <| \_ ->
    Do.env "SESSION_COOKIE" <| \sessionCookie ->
    Do.do
        (File.rawFile path
            |> BackendTask.allowFatal
            |> BackendTask.onError
                (\_ ->
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
                    <| \input ->
                    Do.allowFatal (Script.writeFile { path = path, body = input }) <| \_ ->
                    BackendTask.succeed input
                )
        )
    <| \input ->
    Do.do BackendTask.Time.now <| \before ->
    Do.do (parse input) <| \parsedInput ->
    Do.do BackendTask.Time.now <| \afterParser ->
    Do.log ("Parsed in " ++ timeDiff before afterParser) <| \_ ->
    let
        solution1 : Int
        solution1 =
            solver1 parsedInput
    in
    Do.do BackendTask.Time.now <| \after1 ->
    Do.log ("Solution (part 1): " ++ String.fromInt solution1 ++ " [" ++ timeDiff afterParser after1 ++ "]") <| \_ ->
    let
        solution2 : Int
        solution2 =
            solver2 parsedInput
    in
    Do.do BackendTask.Time.now <| \after2 ->
    Script.log ("Solution (part 2): " ++ String.fromInt solution2 ++ " [" ++ timeDiff after1 after2 ++ "]")
