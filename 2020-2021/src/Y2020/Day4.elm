module Y2020.Day4 exposing (parser, process, processGold)

import Dict exposing (Dict)
import List.Extra
import Parser exposing ((|.), (|=), Parser)
import Set


type alias Item =
    Dict String String


parser : Parser Item
parser =
    Parser.sequence
        { start = ""
        , end = ""
        , spaces = Parser.spaces
        , separator = ""
        , trailing = Parser.Optional
        , item = keyValuePair
        }
        |> Parser.map Dict.fromList


keyValuePair : Parser ( String, String )
keyValuePair =
    Parser.succeed Tuple.pair
        |= Parser.variable
            { start = Char.isAlpha
            , inner = Char.isAlpha
            , reserved = Set.empty
            }
        |. Parser.symbol ":"
        |= Parser.variable
            { start = (/=) ' '
            , inner = (/=) ' '
            , reserved = Set.empty
            }


aggreggate : List Item -> List Item
aggreggate list =
    list
        |> List.Extra.groupWhile (\a b -> Dict.isEmpty a == Dict.isEmpty b)
        |> List.map (\( h, t ) -> List.foldr Dict.union h t)
        |> List.Extra.filterNot Dict.isEmpty


process : List Item -> Int
process list =
    list
        |> aggreggate
        |> List.Extra.count
            (\d ->
                List.all
                    (\k -> Dict.member k d)
                    [ "byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid" ]
            )


processGold : List Item -> Int
processGold list =
    let
        intValidator f s =
            case String.toInt s of
                Nothing ->
                    False

                Just i ->
                    f i

        between low high i =
            low <= i && i <= high

        eyeColors =
            Set.fromList [ "amb", "blu", "brn", "gry", "grn", "hzl", "oth" ]
    in
    list
        |> aggreggate
        |> List.Extra.count
            (\d ->
                List.all
                    (\( k, validator ) ->
                        case Dict.get k d of
                            Nothing ->
                                False

                            Just value ->
                                validator value
                    )
                    [ ( "byr", intValidator (between 1920 2002) )
                    , ( "iyr", intValidator (between 2010 2020) )
                    , ( "eyr", intValidator (between 2020 2030) )
                    , ( "hgt"
                      , \hgt ->
                            if String.endsWith "cm" hgt then
                                intValidator (between 150 193) (String.dropRight 2 hgt)

                            else if String.endsWith "in" hgt then
                                intValidator (between 59 76) (String.dropRight 2 hgt)

                            else
                                False
                      )
                    , ( "hcl"
                      , \s ->
                            (String.length s == 7)
                                && String.startsWith "#" s
                                && List.all
                                    (\c ->
                                        Char.isDigit c
                                            || between
                                                (Char.toCode 'a')
                                                (Char.toCode 'f')
                                                (Char.toCode c)
                                    )
                                    (List.drop 1 <| String.toList s)
                      )
                    , ( "ecl", \ecl -> Set.member ecl eyeColors )
                    , ( "pid", \pid -> String.length pid == 9 && String.toInt pid /= Nothing )
                    ]
            )
