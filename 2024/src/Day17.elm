module Day17 exposing (run)

import Array exposing (Array)
import BackendTask exposing (BackendTask)
import Bitwise
import FatalError exposing (FatalError)
import List.Extra
import Pages.Script as Script exposing (Script)
import Parser exposing ((|.), (|=), Parser)
import Utils


example1 : String
example1 =
    """Register A: 0
Register B: 0
Register C: 9

Program: 2,6,5,5"""


example2 : String
example2 =
    """Register A: 2024
Register B: 0
Register C: 0

Program: 0,3,5,4,3,0"""


exampleLast : String
exampleLast =
    """Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0"""


run : Script
run =
    Script.withoutCliOptions task


task : BackendTask FatalError ()
task =
    Utils.runString
        { day = 17
        , examples = []

        -- [ ( example1, "1", "" )
        -- , ( example2, "5,7,3,0", "117440" )
        -- , ( exampleLast, "4,6,3,5,6,3,5,2,1,0", "" )
        -- ]
        , parser = parser
        , solver1 = part1
        , solver2 = part2
        }


type alias Machine =
    { a : Int
    , b : Int
    , c : Int
    , program : Array Int
    }


parser : Parser Machine
parser =
    Parser.succeed Machine
        |. Parser.symbol "Register A: "
        |= Parser.int
        |. Parser.spaces
        |. Parser.symbol "Register B: "
        |= Parser.int
        |. Parser.spaces
        |. Parser.symbol "Register C: "
        |= Parser.int
        |. Parser.spaces
        |. Parser.symbol "Program: "
        |= (Parser.sequence
                { start = ""
                , end = ""
                , separator = ","
                , spaces = Parser.succeed ()
                , trailing = Parser.Forbidden
                , item = Parser.int
                }
                |> Parser.map Array.fromList
           )


part1 : Machine -> String
part1 machine =
    runMachine1 0 machine.a machine.b machine.c machine.program []
        |> List.map String.fromInt
        |> String.join ","


runMachine1 : Int -> Int -> Int -> Int -> Array Int -> List Int -> List Int
runMachine1 ip a b c program output =
    case ( Array.get ip program, Array.get (ip + 1) program ) of
        ( Nothing, _ ) ->
            List.reverse output

        ( Just 0, Just operand ) ->
            runMachine1 (ip + 2) (a // pow a b c operand) b c program output

        ( Just 1, Just operand ) ->
            runMachine1 (ip + 2) a (Bitwise.xor b operand) c program output

        ( Just 2, Just operand ) ->
            runMachine1 (ip + 2) a (modBy 8 (comboValue a b c operand)) c program output

        ( Just 3, Just operand ) ->
            if a == 0 then
                runMachine1 (ip + 2) a b c program output

            else
                runMachine1 operand a b c program output

        ( Just 4, Just _ ) ->
            runMachine1 (ip + 2) a (Bitwise.xor b c) c program output

        ( Just 5, Just operand ) ->
            runMachine1 (ip + 2) a b c program (modBy 8 (comboValue a b c operand) :: output)

        ( Just 6, Just operand ) ->
            runMachine1 (ip + 2) a (a // pow a b c operand) c program output

        ( Just 7, Just operand ) ->
            runMachine1 (ip + 2) a b (a // pow a b c operand) program output

        ( Just opcode, Just _ ) ->
            Debug.todo ("Missing opcode: " ++ String.fromInt opcode)

        ( _, Nothing ) ->
            Debug.todo "Reading off tape"


part2 : Machine -> String
part2 machine =
    let
        _ =
            Debug.log ("\n\n" ++ dump machine) ()

        expected : List Int
        expected =
            machine.program
                |> Array.toList

        go : Int -> String
        go a =
            if runMachine2 0 a machine.b machine.c machine.program expected then
                String.fromInt a

            else
                let
                    _ =
                        if modBy 1000000 a == 0 then
                            Debug.log "a" a

                        else
                            a
                in
                go (a + 1)
    in
    go
        (machine.program
            |> Array.foldr (\d acc -> acc * 8 + Bitwise.xor d 3) 0
        )


dump : Machine -> String
dump { program } =
    program
        |> Array.toList
        |> List.Extra.greedyGroupsOf 2
        |> List.filterMap
            (\group ->
                case group of
                    [ opcode, operand ] ->
                        Just (dumpOpcode opcode operand)

                    _ ->
                        Nothing
            )
        |> String.join "\n"


dumpOpcode : Int -> Int -> String
dumpOpcode opcode operand =
    case opcode of
        0 ->
            "a /= 2 ^ " ++ dumpCombo operand

        1 ->
            "b ^= " ++ String.fromInt operand

        2 ->
            "b = " ++ dumpCombo operand ++ " % 8"

        3 ->
            "jnz " ++ String.fromInt operand

        4 ->
            "b ^= c"

        5 ->
            "out " ++ dumpCombo operand ++ " % 8"

        6 ->
            "b = a / 2 ^ " ++ dumpCombo operand

        7 ->
            "c = a / 2 ^ " ++ dumpCombo operand

        _ ->
            "UNKNOWN"


dumpCombo : Int -> String
dumpCombo operand =
    case operand of
        0 ->
            "0"

        1 ->
            "1"

        2 ->
            "2"

        3 ->
            "3"

        4 ->
            "a"

        5 ->
            "b"

        6 ->
            "c"

        7 ->
            "RESERVED"

        _ ->
            "UNKNOWN"


runMachine2 : Int -> Int -> Int -> Int -> Array Int -> List Int -> Bool
runMachine2 ip a b c program expected =
    case ( Array.get ip program, Array.get (ip + 1) program ) of
        ( Nothing, _ ) ->
            List.isEmpty expected

        ( Just 0, Just operand ) ->
            runMachine2 (ip + 2) (a // pow a b c operand) b c program expected

        ( Just 1, Just operand ) ->
            runMachine2 (ip + 2) a (Bitwise.xor b operand) c program expected

        ( Just 2, Just operand ) ->
            runMachine2 (ip + 2) a (modBy 8 (comboValue a b c operand)) c program expected

        ( Just 3, Just operand ) ->
            if a == 0 then
                runMachine2 (ip + 2) a b c program expected

            else
                runMachine2 operand a b c program expected

        ( Just 4, Just _ ) ->
            runMachine2 (ip + 2) a (Bitwise.xor b c) c program expected

        ( Just 5, Just operand ) ->
            case expected of
                [] ->
                    False

                head :: tail ->
                    let
                        out : Int
                        out =
                            modBy 8 (comboValue a b c operand)
                    in
                    if out == head then
                        runMachine2 (ip + 2) a b c program tail

                    else
                        False

        ( Just 6, Just operand ) ->
            runMachine2 (ip + 2) a (a // pow a b c operand) c program expected

        ( Just 7, Just operand ) ->
            runMachine2 (ip + 2) a b (a // pow a b c operand) program expected

        ( Just opcode, Just _ ) ->
            Debug.todo ("Missing opcode: " ++ String.fromInt opcode)

        ( _, Nothing ) ->
            Debug.todo "Reading off tape"


pow : Int -> Int -> Int -> Int -> Int
pow a b c operand =
    let
        val : Int
        val =
            comboValue a b c operand
    in
    if val > 32 then
        Debug.todo "Value too big, might explode"

    else
        2 ^ val


comboValue : Int -> Int -> Int -> Int -> Int
comboValue a b c combo =
    case combo of
        0 ->
            0

        1 ->
            1

        2 ->
            2

        3 ->
            3

        4 ->
            a

        5 ->
            b

        6 ->
            c

        7 ->
            Debug.todo "Reserved"

        _ ->
            Debug.todo ("Invalid combo value: " ++ String.fromInt combo)
