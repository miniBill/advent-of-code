module Y2023.Day5 exposing (gold, silver)

import Parser exposing ((|.), (|=), Parser)
import Utils


silver : String -> Result String String
silver =
    Utils.withParser parser
        (\{ seeds, maps } ->
            List.foldl
                (\map acc ->
                    let
                        ( unmapped, mapped ) =
                            List.foldl
                                (\item ( uacc, macc ) ->
                                    List.foldl
                                        (\value ( nuacc, nmacc ) ->
                                            if value >= item.sourceStart && value < item.sourceStart + item.length then
                                                let
                                                    mappedValue : Int
                                                    mappedValue =
                                                        item.destStart - item.sourceStart + value
                                                in
                                                ( nuacc, mappedValue :: nmacc )

                                            else
                                                ( value :: nuacc, nmacc )
                                        )
                                        ( [], macc )
                                        uacc
                                )
                                ( acc, [] )
                                map
                    in
                    unmapped ++ mapped
                )
                seeds
                maps
                |> List.minimum
                |> Result.fromMaybe "Empty list of seeds?"
                |> Result.map String.fromInt
        )


gold : String -> Result String String
gold =
    Utils.withParser parser
        (\{ seeds, maps } ->
            Err "TODO"
        )


type alias File =
    { seeds : List Int
    , maps : List Map
    }


type alias Map =
    List Item


type alias Item =
    { destStart : Int
    , sourceStart : Int
    , length : Int
    }


parser : Parser File
parser =
    Parser.succeed File
        |. Parser.symbol "seeds:"
        |. Parser.spaces
        |= Parser.sequence
            { start = ""
            , end = ""
            , separator = ""
            , spaces = Parser.spaces
            , item = Parser.int
            , trailing = Parser.Optional
            }
        |. Parser.spaces
        |= Parser.sequence
            { start = ""
            , end = ""
            , separator = ""
            , spaces = Parser.spaces
            , item = mapParser
            , trailing = Parser.Optional
            }


mapParser : Parser Map
mapParser =
    Parser.succeed identity
        |. Parser.chompWhile ((/=) ':')
        |. Parser.symbol ":"
        |= Parser.sequence
            { start = ""
            , end = ""
            , separator = ""
            , spaces = Parser.spaces
            , item = itemParser
            , trailing = Parser.Optional
            }


itemParser : Parser Item
itemParser =
    Parser.succeed Item
        |= Parser.int
        |. Parser.spaces
        |= Parser.int
        |. Parser.spaces
        |= Parser.int
