module Y2021.Day4 exposing (parser, process, processGold)

import List.Extra
import Parser exposing ((|.), (|=), Parser)
import Parser.Extras


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
        |= Parser.Extras.many boardParser


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


boardParser : Parser Board
boardParser =
    Parser.succeed (\a b c d e -> [ a, b, c, d, e ])
        |. Parser.symbol "\n"
        |= lineParser
        |= lineParser
        |= lineParser
        |= lineParser
        |= lineParser


lineParser : Parser Line
lineParser =
    Parser.sequence
        { start = ""
        , spaces = Parser.spaces
        , separator = ""
        , end = "\n"
        , item = Parser.map (Tuple.pair False) Parser.int
        , trailing = Parser.Optional
        }


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
                    winning
                        |> List.concatMap (List.Extra.filterNot Tuple.first)
                        |> List.map Tuple.second
                        |> List.sum
                        |> (*) n
                        |> String.fromInt

                Nothing ->
                    processHelper ns boards_


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
processGold _ =
    "TODO"
