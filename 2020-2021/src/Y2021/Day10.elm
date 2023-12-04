module Y2021.Day10 exposing (parser, process, processGold)

import Parser exposing ((|.), (|=), Parser)


type alias Item =
    List Char


parser : Parser Item
parser =
    Parser.succeed identity
        |. Parser.chompWhile ((/=) '\n')
        |> Parser.getChompedString
        |> Parser.map String.toList


process : List Item -> { output : String, log : List String }
process lines =
    let
        ( scores, logs ) =
            List.unzip <| List.map scoreLineWrong lines
    in
    { output = String.fromInt <| List.sum scores
    , log = List.map3 (\l s lg -> String.fromList l ++ ": " ++ String.fromInt s ++ " -- " ++ lg) lines scores logs
    }


scoreLineWrong : Item -> ( Int, String )
scoreLineWrong =
    let
        charToString c =
            if c == '\n' then
                "'\\n'"

            else
                "'" ++ String.fromChar c ++ "'"

        go expected line =
            case ( expected, line ) of
                ( _, [] ) ->
                    ( 0, "Incomplete, expected " ++ String.fromList expected )

                ( _, '{' :: ls ) ->
                    go ('}' :: expected) ls

                ( _, '<' :: ls ) ->
                    go ('>' :: expected) ls

                ( _, '[' :: ls ) ->
                    go (']' :: expected) ls

                ( _, '(' :: ls ) ->
                    go (')' :: expected) ls

                ( [], l :: _ ) ->
                    ( wrongScore l, "Unexpected " ++ charToString l )

                ( e :: es, l :: ls ) ->
                    if l == e then
                        go es ls

                    else
                        ( wrongScore l
                        , "Unexpected " ++ charToString l ++ ", expected " ++ charToString e
                        )
    in
    go []


wrongScore : Char -> Int
wrongScore l =
    case l of
        ')' ->
            3

        ']' ->
            57

        '}' ->
            1197

        '>' ->
            25137

        _ ->
            -1


processGold : List Item -> { output : String, log : List String }
processGold lines =
    let
        ( scores, logs ) =
            List.unzip <| List.map scoreLineIncomplete lines

        sorted =
            scores |> List.filter ((/=) 0) |> List.sort
    in
    { output = String.fromInt <| List.sum <| List.take 1 <| List.drop (List.length sorted // 2) sorted
    , log = List.map3 (\l s lg -> String.fromList l ++ ": " ++ String.fromInt s ++ " -- " ++ lg) lines scores logs
    }


scoreLineIncomplete : Item -> ( Int, String )
scoreLineIncomplete =
    let
        charToString c =
            if c == '\n' then
                "'\\n'"

            else
                "'" ++ String.fromChar c ++ "'"

        go expected line =
            case ( expected, line ) of
                ( _, [] ) ->
                    ( calculateCompletionScore expected, "Incomplete, expected " ++ String.fromList expected )

                ( _, '{' :: ls ) ->
                    go ('}' :: expected) ls

                ( _, '<' :: ls ) ->
                    go ('>' :: expected) ls

                ( _, '[' :: ls ) ->
                    go (']' :: expected) ls

                ( _, '(' :: ls ) ->
                    go (')' :: expected) ls

                ( [], l :: _ ) ->
                    ( 0, "Unexpected " ++ charToString l )

                ( e :: es, l :: ls ) ->
                    if l == e then
                        go es ls

                    else
                        ( 0, "Unexpected " ++ charToString l ++ ", expected " ++ charToString e )
    in
    go []


calculateCompletionScore : List Char -> number
calculateCompletionScore =
    List.foldl
        (\e acc ->
            let
                v =
                    case e of
                        ')' ->
                            1

                        ']' ->
                            2

                        '}' ->
                            3

                        '>' ->
                            4

                        _ ->
                            Debug.todo "Unexpected"
            in
            acc * 5 + v
        )
        0
