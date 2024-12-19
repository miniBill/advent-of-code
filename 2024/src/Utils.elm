module Utils exposing (run, runLineBased, runString)

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
    runString
        { day = day
        , examples =
            examples
                |> List.map
                    (\( input, output1, output2 ) ->
                        ( input
                        , if output1 == -1 then
                            ""

                          else
                            String.fromInt output1
                        , if output2 == -1 then
                            ""

                          else
                            String.fromInt output2
                        )
                    )
        , parser = parser
        , solver1 = \input -> input |> solver1 |> String.fromInt
        , solver2 = \input -> input |> solver2 |> String.fromInt
        }


runString :
    { day : Int
    , examples : List ( String, String, String )
    , parser : Parser input
    , solver1 : input -> String
    , solver2 : input -> String
    }
    -> BackendTask FatalError ()
runString { day, examples, parser, solver1, solver2 } =
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
        (\( parsedExample, exampleSolution1, _ ) ->
            if String.isEmpty exampleSolution1 then
                BackendTask.succeed ()

            else
                let
                    exampleActual1 : String
                    exampleActual1 =
                        solver1 parsedExample
                in
                if exampleActual1 /= exampleSolution1 then
                    BackendTask.fail (FatalError.fromString ("(part1) Expected example solution to be " ++ exampleSolution1 ++ " but got " ++ exampleActual1))

                else
                    BackendTask.succeed ()
        )
    <| \_ ->
    Do.each parsedExamples
        (\( parsedExample, _, exampleSolution2 ) ->
            if String.isEmpty exampleSolution2 then
                BackendTask.succeed ()

            else
                let
                    exampleActual2 : String
                    exampleActual2 =
                        solver2 parsedExample
                in
                if exampleActual2 /= exampleSolution2 then
                    BackendTask.fail (FatalError.fromString ("(part2) Expected example solution to be " ++ exampleSolution2 ++ " but got " ++ exampleActual2))

                else
                    BackendTask.succeed ()
        )
    <| \_ ->
    Do.log "Examples check out" <| \_ ->
    Do.do
        (File.rawFile path
            |> BackendTask.allowFatal
            |> BackendTask.onError
                (\_ ->
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
        solution1 : String
        solution1 =
            solver1 parsedInput
    in
    Do.do BackendTask.Time.now <| \after1 ->
    Do.log ("Solution (part 1): " ++ solution1 ++ " [" ++ timeDiff afterParser after1 ++ "]") <| \_ ->
    let
        solution2 : String
        solution2 =
            solver2 parsedInput
    in
    Do.do BackendTask.Time.now <| \after2 ->
    Script.log ("Solution (part 2): " ++ solution2 ++ " [" ++ timeDiff after1 after2 ++ "]")
