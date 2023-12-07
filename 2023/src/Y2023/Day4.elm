module Y2023.Day4 exposing (gold, silver)

import Dict exposing (Dict)
import Parser exposing ((|.), (|=), Parser)
import Set
import Utils


silver : String -> Result String String
silver =
    Utils.perLineWithParser parser
        (\cards ->
            cards
                |> List.map scoreSilver
                |> List.sum
                |> String.fromInt
                |> Ok
        )


scoreSilver : Card -> Int
scoreSilver card =
    let
        exp : Int
        exp =
            matches card
    in
    if exp == 0 then
        0

    else
        2 ^ (exp - 1)


matches : Card -> Int
matches card =
    Set.fromList card.have
        |> Set.intersect (Set.fromList card.winning)
        |> Set.size


gold : String -> Result String String
gold =
    Utils.perLineWithParser parser
        (\cards ->
            let
                initialCounts : Dict Int Int
                initialCounts =
                    List.repeat (List.length cards) ()
                        |> List.indexedMap (\index _ -> ( index + 1, 1 ))
                        |> Dict.fromList
            in
            cards
                |> List.foldl
                    (\card counts ->
                        List.range
                            (card.index + 1)
                            (card.index + matches card)
                            |> List.foldl
                                (\toAdd ->
                                    Dict.update toAdd
                                        (Maybe.map
                                            ((+)
                                                (Maybe.withDefault 0 <|
                                                    Dict.get card.index counts
                                                )
                                            )
                                        )
                                )
                                counts
                    )
                    initialCounts
                |> Dict.values
                |> List.sum
                |> String.fromInt
                |> Ok
        )


type alias Card =
    { index : Int
    , winning : List Int
    , have : List Int
    }


parser : Parser Card
parser =
    Parser.succeed Card
        |. Parser.symbol "Card"
        |. Parser.spaces
        |= Parser.int
        |. Parser.symbol ":"
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
        |. Parser.symbol "|"
        |. Parser.spaces
        |= Parser.sequence
            { start = ""
            , end = ""
            , separator = ""
            , spaces = Parser.spaces
            , item = Parser.int
            , trailing = Parser.Mandatory
            }
