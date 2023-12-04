module Y2023.Day2 exposing (gold, silver)

import Parser exposing ((|.), (|=), Parser)
import Utils


silver : String -> Result String String
silver =
    Utils.perLineWithParser gameParser <|
        \games ->
            games
                |> List.filter (filterPossibleGame 12 13 14)
                |> List.map Tuple.first
                |> Utils.justSum


filterPossibleGame : Int -> Int -> Int -> Game -> Bool
filterPossibleGame maxRed maxGreen maxBlue ( id, rounds ) =
    let
        ( red, green, blue ) =
            foldMax rounds
    in
    red <= maxRed && green <= maxGreen && blue <= maxBlue


foldMax : List ( Int, Int, Int ) -> ( Int, Int, Int )
foldMax list =
    List.foldl max3 ( 0, 0, 0 ) list


gold : String -> Result String String
gold =
    Utils.perLineWithParser gameParser <|
        \games ->
            games
                |> List.map
                    (\( _, game ) ->
                        let
                            ( r, g, b ) =
                                foldMax game
                        in
                        r * g * b
                    )
                |> Utils.justSum



-- Generic


max3 : ( Int, Int, Int ) -> ( Int, Int, Int ) -> ( Int, Int, Int )
max3 ( r, g, b ) ( ar, ag, ab ) =
    ( max r ar, max g ag, max b ab )


type alias Game =
    ( Int, List ( Int, Int, Int ) )


gameParser : Parser Game
gameParser =
    Parser.succeed Tuple.pair
        |. Parser.symbol "Game "
        |= Parser.int
        |. Parser.symbol ": "
        |= Parser.sequence
            { start = ""
            , end = ""
            , separator = ";"
            , spaces = Parser.spaces
            , trailing = Parser.Forbidden
            , item = roundParser
            }


roundParser : Parser ( Int, Int, Int )
roundParser =
    Parser.succeed foldMax
        |= Parser.sequence
            { start = ""
            , end = ""
            , spaces = Parser.spaces
            , separator = ","
            , trailing = Parser.Forbidden
            , item =
                Parser.succeed (\x f -> f x)
                    |= Parser.int
                    |. Parser.spaces
                    |= Parser.oneOf
                        [ Parser.succeed (\r -> ( r, 0, 0 )) |. Parser.symbol "red"
                        , Parser.succeed (\g -> ( 0, g, 0 )) |. Parser.symbol "green"
                        , Parser.succeed (\b -> ( 0, 0, b )) |. Parser.symbol "blue"
                        ]
            }
