module Utils exposing (deadEndsToString, justSum, lines, perLineWith, perLineWithParser)

import Parser exposing (Parser)
import Result.Extra


justSum : List Int -> Result error String
justSum list =
    Ok <| String.fromInt <| List.sum list


perLineWith : (List b -> Result String a) -> (String -> Result String b) -> String -> Result String a
perLineWith final perLine input =
    input
        |> lines
        |> Result.Extra.combineMap
            (\line ->
                line
                    |> perLine
                    |> Result.mapError
                        (\e -> e ++ "\n  for line " ++ line)
            )
        |> Result.andThen final


lines : String -> List String
lines input =
    input
        |> String.lines
        |> List.filter (\s -> not (String.isEmpty s))


perLineWithParser : Parser a -> (List a -> Result String b) -> String -> Result String b
perLineWithParser parser final input =
    perLineWith final
        (\line ->
            line
                |> Parser.run parser
                |> Result.mapError deadEndsToString
        )
        input


deadEndsToString : List Parser.DeadEnd -> String
deadEndsToString deadEnds =
    String.concat (List.intersperse "; " (List.map deadEndToString deadEnds))


deadEndToString : Parser.DeadEnd -> String
deadEndToString deadend =
    problemToString deadend.problem ++ " at row " ++ String.fromInt deadend.row ++ ", col " ++ String.fromInt deadend.col


problemToString : Parser.Problem -> String
problemToString p =
    case p of
        Parser.Expecting s ->
            "expecting '" ++ s ++ "'"

        Parser.ExpectingInt ->
            "expecting int"

        Parser.ExpectingHex ->
            "expecting hex"

        Parser.ExpectingOctal ->
            "expecting octal"

        Parser.ExpectingBinary ->
            "expecting binary"

        Parser.ExpectingFloat ->
            "expecting float"

        Parser.ExpectingNumber ->
            "expecting number"

        Parser.ExpectingVariable ->
            "expecting variable"

        Parser.ExpectingSymbol s ->
            "expecting symbol '" ++ s ++ "'"

        Parser.ExpectingKeyword s ->
            "expecting keyword '" ++ s ++ "'"

        Parser.ExpectingEnd ->
            "expecting end"

        Parser.UnexpectedChar ->
            "unexpected char"

        Parser.Problem s ->
            "problem " ++ s

        Parser.BadRepeat ->
            "bad repeat"
