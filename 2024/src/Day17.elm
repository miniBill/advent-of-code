module Day17 exposing (run)

import Array exposing (Array)
import BackendTask exposing (BackendTask)
import Bitwise
import FatalError exposing (FatalError)
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
        , examples =
            [ ( example1, "1", "" )
            , ( example2, "5,7,3,0", "117440" )
            , ( exampleLast, "4,6,3,5,6,3,5,2,1,0", "" )
            ]
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
        expected : List Int
        expected =
            machine.program
                |> Array.toList

        go : Int -> String
        go a =
            if runMachine2 1000000 0 a machine.b machine.c machine.program expected then
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
    go 0


runMachine2 : Int -> Int -> Int -> Int -> Int -> Array Int -> List Int -> Bool
runMachine2 budget ip a b c program expected =
    if budget <= 0 then
        let
            _ =
                Debug.log "Ran out of budget" ()
        in
        False

    else
        case ( Array.get ip program, Array.get (ip + 1) program ) of
            ( Nothing, _ ) ->
                List.isEmpty expected

            ( Just 0, Just operand ) ->
                runMachine2 (budget - 1) (ip + 2) (a // pow a b c operand) b c program expected

            ( Just 1, Just operand ) ->
                runMachine2 (budget - 1) (ip + 2) a (Bitwise.xor b operand) c program expected

            ( Just 2, Just operand ) ->
                runMachine2 (budget - 1) (ip + 2) a (modBy 8 (comboValue a b c operand)) c program expected

            ( Just 3, Just operand ) ->
                if a == 0 then
                    runMachine2 (budget - 1) (ip + 2) a b c program expected

                else
                    runMachine2 (budget - 1) operand a b c program expected

            ( Just 4, Just _ ) ->
                runMachine2 (budget - 1) (ip + 2) a (Bitwise.xor b c) c program expected

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
                            runMachine2 (budget - 1) (ip + 2) a b c program tail

                        else
                            False

            ( Just 6, Just operand ) ->
                runMachine2 (budget - 1) (ip + 2) a (a // pow a b c operand) c program expected

            ( Just 7, Just operand ) ->
                runMachine2 (budget - 1) (ip + 2) a b (a // pow a b c operand) program expected

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
