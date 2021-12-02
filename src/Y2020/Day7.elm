module Y2020.Day7 exposing (parser, process, processGold)

import Dict
import List.Extra
import Memoize
import Parser exposing ((|.), (|=), Parser)
import Set


type alias Item =
    ( String, List ( Int, String ) )


name : Parser String
name =
    Parser.variable
        { start = Char.isAlpha
        , inner = Char.isAlpha
        , reserved = Set.empty
        }


parser : Parser Item
parser =
    let
        item =
            Parser.succeed Tuple.pair
                |= Parser.int
                |. Parser.spaces
                |= twoNames
                |. Parser.spaces
                |. Parser.oneOf
                    [ Parser.keyword "bags"
                    , Parser.keyword "bag"
                    ]

        twoNames =
            Parser.getChompedString
                (Parser.succeed ()
                    |. name
                    |. Parser.spaces
                    |. name
                )
    in
    Parser.succeed Tuple.pair
        |= twoNames
        |. Parser.spaces
        |. Parser.keyword "bags"
        |. Parser.spaces
        |. Parser.keyword "contain"
        |. Parser.spaces
        |= Parser.oneOf
            [ Parser.succeed []
                |. Parser.symbol "no other bags."
            , Parser.sequence
                { start = ""
                , end = "."
                , separator = ","
                , spaces = Parser.spaces
                , item = item
                , trailing = Parser.Forbidden
                }
            ]
        |. Parser.end


process : List Item -> Int
process list =
    let
        dict =
            Dict.fromList list

        go key acc =
            case Dict.get key acc of
                Just can ->
                    ( can, acc )

                Nothing ->
                    case Dict.get key dict of
                        Nothing ->
                            ( False, acc )

                        Just children ->
                            children
                                |> List.foldl
                                    (\( _, child ) ( can, iacc ) ->
                                        if can then
                                            ( can, iacc )

                                        else if child == "shiny gold" then
                                            ( True, Dict.insert key True acc )

                                        else
                                            let
                                                ( childCan, childAcc ) =
                                                    go child iacc
                                            in
                                            if childCan then
                                                ( childCan, Dict.insert key childCan childAcc )

                                            else
                                                ( childCan, childAcc )
                                    )
                                    ( False, acc )
    in
    Dict.keys dict
        |> List.foldr
            (\key acc ->
                if Dict.member key acc then
                    acc

                else
                    Tuple.second <| go key acc
            )
            Dict.empty
        |> Dict.values
        |> List.Extra.count identity


processGold : List Item -> Int
processGold list =
    let
        dict =
            Dict.fromList list

        go key =
            case Dict.get key dict of
                Nothing ->
                    ( [], always 0 )

                Just children ->
                    ( List.map Tuple.second children
                    , \memo ->
                        children
                            |> List.map
                                (\( childCount, child ) ->
                                    Dict.get child memo
                                        |> Maybe.withDefault 0
                                        |> (*) childCount
                                )
                            |> List.sum
                            |> (+) 1
                    )
    in
    Memoize.memoized go "shiny gold" - 1
