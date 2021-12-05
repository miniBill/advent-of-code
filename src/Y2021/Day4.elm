module Y2021.Day4 exposing (parser, process, processGold)

import List.Extra
import Parser exposing ((|.), (|=), Parser)


type alias Item =
    { header : List Int
    , boards : List Board
    }


type alias Board =
    List Line


type alias Line =
    List Cell


type alias Cell =
    ( Bool, Int )


parser : Parser Item
parser =
    Parser.succeed (\header boards -> { header = header, boards = boards })
        |= headerParser
        |. Parser.symbol "\n"
        |= Parser.map parseBoards (Parser.getChompedString <| Parser.chompUntilEndOr "!!!")


headerParser : Parser (List Int)
headerParser =
    Parser.sequence
        { start = ""
        , end = "\n"
        , spaces = Parser.succeed ()
        , item = Parser.int
        , separator = ","
        , trailing = Parser.Forbidden
        }


parseBoards : String -> List Board
parseBoards input =
    input
        |> String.split "\n\n"
        |> List.map parseBoard


parseBoard : String -> Board
parseBoard input =
    input
        |> String.split "\n"
        |> List.map parseLine


parseLine : String -> Line
parseLine input =
    input
        |> String.split " "
        |> List.map String.trim
        |> List.Extra.filterNot String.isEmpty
        |> List.filterMap String.toInt
        |> List.map (Tuple.pair False)


process : Item -> String
process { header, boards } =
    processHelper header boards


processHelper : List Int -> List Board -> String
processHelper numbers boards =
    case numbers of
        [] ->
            "No winning board"

        n :: ns ->
            let
                boards_ =
                    List.map (mark n) boards
            in
            case List.Extra.find isWinningBoard boards_ of
                Just winning ->
                    score winning n

                Nothing ->
                    processHelper ns boards_


score : Board -> Int -> String
score winning n =
    winning
        |> List.concatMap (List.Extra.filterNot Tuple.first)
        |> List.map Tuple.second
        |> List.sum
        |> (*) n
        |> String.fromInt


isWinningBoard : Board -> Bool
isWinningBoard board =
    let
        hasWinningRow =
            List.any (List.all Tuple.first)
    in
    hasWinningRow board
        || hasWinningRow (List.Extra.transpose board)


mark : Int -> Board -> Board
mark n =
    List.map (List.map (\( m, i ) -> ( m || i == n, i )))


processGold : Item -> String
processGold { header, boards } =
    processGoldHelper header boards


processGoldHelper : List Int -> List Board -> String
processGoldHelper numbers boards =
    case numbers of
        [] ->
            "Some boards never win"

        n :: ns ->
            let
                boards_ =
                    List.map (mark n) boards
            in
            case boards_ of
                [ b ] ->
                    if isWinningBoard b then
                        score b n

                    else
                        processGoldHelper ns boards_

                _ ->
                    processGoldHelper ns (List.Extra.filterNot isWinningBoard boards_)
