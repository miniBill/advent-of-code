module Y2020.Day1 exposing (parser, process, processGold)

import Parser exposing (Parser)
import Set


type alias Item =
    Int


parser : Parser Item
parser =
    Parser.int


process : List Item -> Int
process list =
    let
        set =
            Set.fromList list
    in
    list
        |> List.foldl
            (\e acc ->
                if acc == 0 && Set.member (2020 - e) set then
                    e * (2020 - e)

                else
                    acc
            )
            0


processGold : List Item -> Int
processGold list =
    let
        set =
            Set.fromList list
    in
    List.foldl
        (\e oacc ->
            if oacc == 0 then
                List.foldl
                    (\f iacc ->
                        if iacc == 0 && Set.member (2020 - e - f) set then
                            e * f * (2020 - e - f)

                        else
                            iacc
                    )
                    0
                    list

            else
                oacc
        )
        0
        list
