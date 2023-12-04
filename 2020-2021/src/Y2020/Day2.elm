module Y2020.Day2 exposing (parser, process, processGold)

import List.Extra
import Parser exposing ((|.), (|=), Parser)


type alias Item =
    { min : Int
    , max : Int
    , char : Char
    , password : String
    }


parser : Parser Item
parser =
    Parser.succeed Item
        |= Parser.int
        |. Parser.symbol "-"
        |= Parser.int
        |. Parser.spaces
        |= (Parser.chompIf (always True)
                |> Parser.getChompedString
                |> Parser.andThen
                    (\s ->
                        case String.toList s of
                            [ c ] ->
                                Parser.succeed c

                            _ ->
                                Parser.problem "wrong string length for a single char???"
                    )
           )
        |. Parser.symbol ":"
        |. Parser.spaces
        |= Parser.getChompedString (Parser.chompWhile (always True))


process : List Item -> Int
process =
    List.Extra.count
        (\{ min, max, char, password } ->
            let
                count =
                    String.toList password |> List.Extra.count ((==) char)
            in
            count >= min && count <= max
        )


processGold : List Item -> Int
processGold =
    List.Extra.count
        (\{ min, max, char, password } ->
            let
                charAsString =
                    String.fromChar char
            in
            (String.slice (min - 1) min password == charAsString)
                /= (String.slice (max - 1) max password == charAsString)
        )
