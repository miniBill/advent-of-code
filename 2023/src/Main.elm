module Main exposing (run)

import BackendTask exposing (BackendTask)
import BackendTask.Env
import BackendTask.Http as Http
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import Day exposing (Day)
import FatalError exposing (FatalError)
import Pages.Script as Script exposing (Script)
import Star exposing (Star(..))
import Url.Builder
import Y2023


run : Script
run =
    Script.withCliOptions config
        (\{ year, day, star } ->
            let
                url : String
                url =
                    Url.Builder.crossOrigin
                        "https://adventofcode.com"
                        [ String.fromInt year
                        , "day"
                        , String.fromInt (Day.toInt day)
                        , "input"
                        ]
                        []

                input : BackendTask FatalError String
                input =
                    BackendTask.Env.expect "SESSION_COOKIE"
                        |> BackendTask.allowFatal
                        |> BackendTask.andThen
                            (\sessionCookie ->
                                Http.getWithOptions
                                    { url = url
                                    , headers =
                                        [ ( "Cookie", "session=" ++ sessionCookie )
                                        ]
                                    , cachePath = Nothing
                                    , cacheStrategy = Nothing
                                    , expect = Http.expectString
                                    , timeoutInMs = Nothing
                                    , retries = Nothing
                                    }
                                    |> BackendTask.allowFatal
                            )

                solver : Result String (Day -> Star -> String -> Result String String)
                solver =
                    case year of
                        2023 ->
                            Ok Y2023.solve

                        _ ->
                            Err <| "Year " ++ String.fromInt year ++ "not supported"
            in
            input
                |> BackendTask.map (\i -> Result.andThen (\s -> s day star i) solver)
                |> BackendTask.andThen
                    (\res ->
                        case res of
                            Ok output ->
                                Script.log output

                            Err msg ->
                                BackendTask.fail <| FatalError.fromString msg
                    )
        )


type alias CliOptions =
    { year : Int
    , day : Day
    , star : Star
    }


config : Program.Config CliOptions
config =
    Program.config
        |> Program.add
            (OptionsParser.build CliOptions
                |> OptionsParser.with
                    (Option.requiredPositionalArg "year"
                        |> Option.validateMap
                            (\yearString ->
                                case String.toInt yearString of
                                    Just year ->
                                        Ok year

                                    Nothing ->
                                        Err <| "Invalid year number: " ++ yearString
                            )
                    )
                |> OptionsParser.with
                    (Option.requiredPositionalArg "day"
                        |> Option.validateMap
                            (\dayString ->
                                case Maybe.andThen Day.fromInt <| String.toInt dayString of
                                    Just day ->
                                        Ok day

                                    Nothing ->
                                        Err <| "Invalid day number: " ++ dayString
                            )
                    )
                |> OptionsParser.with
                    (Option.requiredPositionalArg "star"
                        |> Option.validateMap
                            (\starString ->
                                case String.toLower starString of
                                    "silver" ->
                                        Ok Silver

                                    "gold" ->
                                        Ok Gold

                                    _ ->
                                        Err <| "Invalid star: " ++ starString
                            )
                    )
            )
