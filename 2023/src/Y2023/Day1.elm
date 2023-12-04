module Y2023.Day1 exposing (gold, silver)

import Parser exposing ((|.), (|=), Parser)
import Utils


silver : String -> Result String String
silver =
    Utils.perLineWith Utils.justSum extractSilverNumber


extractSilverNumber : String -> Result String Int
extractSilverNumber line =
    let
        list : List Int
        list =
            line
                |> String.toList
                |> List.filterMap
                    (\c ->
                        c
                            |> String.fromChar
                            |> String.toInt
                    )
    in
    case list of
        [] ->
            Err <| "No digits in line " ++ line

        head :: tail ->
            let
                last =
                    Maybe.withDefault head <| List.head <| List.reverse tail
            in
            Ok <| head * 10 + last


gold : String -> Result String String
gold =
    Utils.perLineWith Utils.justSum extractGoldNumber


extractGoldNumber : String -> Result String Int
extractGoldNumber line =
    Result.map2 (\first last -> first * 10 + last)
        (Parser.run firstParser line)
        (Parser.run lastParser (String.reverse line))
        |> Result.mapError Utils.deadEndsToString


digits : List ( String, number )
digits =
    [ ( "zero", 0 )
    , ( "one", 1 )
    , ( "two", 2 )
    , ( "three", 3 )
    , ( "four", 4 )
    , ( "five", 5 )
    , ( "six", 6 )
    , ( "seven", 7 )
    , ( "eight", 8 )
    , ( "nine", 9 )
    ]


firstParser : Parser Int
firstParser =
    toParser digits


lastParser : Parser Int
lastParser =
    toParser (List.map (Tuple.mapFirst String.reverse) digits)


toParser : List ( String, Int ) -> Parser Int
toParser ds =
    ds
        |> List.concatMap
            (\( token, value ) ->
                [ Parser.succeed value |. Parser.token (String.fromInt value)
                , Parser.succeed value |. Parser.token token
                ]
            )
        |> (\l ->
                l
                    ++ [ Parser.succeed identity
                            |. Parser.chompIf (\_ -> True)
                            |= Parser.lazy (\_ -> toParser ds)
                       ]
           )
        |> Parser.oneOf
