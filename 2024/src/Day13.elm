module Day13 exposing (run)

import BackendTask exposing (BackendTask)
import BigInt exposing (BigInt)
import FatalError exposing (FatalError)
import Pages.Script as Script exposing (Script)
import Parser exposing ((|.), (|=), Parser)
import Utils


example1 : String
example1 =
    """Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400"""


example2 : String
example2 =
    """
Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176"""


example3 : String
example3 =
    """Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450"""


example4 : String
example4 =
    """Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279"""


run : Script
run =
    Script.withoutCliOptions task


task : BackendTask FatalError ()
task =
    Utils.run
        { day = 13
        , examples =
            [ ( example1, 280, 0 )
            , ( example2, 0, 459236326669 )
            , ( example3, 200, 0 )
            , ( example4, 0, 416082282239 )
            ]
        , parser = parser
        , solver1 = part1
        , solver2 = part2
        }


type alias Machine =
    { a : Button
    , b : Button
    , prize : Prize
    }


type alias Button =
    { dx : BigInt
    , dy : BigInt
    }


type alias Prize =
    { x : BigInt
    , y : BigInt
    }


parser : Parser (List Machine)
parser =
    Parser.sequence
        { start = ""
        , end = ""
        , separator = ""
        , spaces = Parser.spaces
        , item = machineParser
        , trailing = Parser.Optional
        }


machineParser : Parser Machine
machineParser =
    Parser.succeed Machine
        |= buttonParser "A"
        |= buttonParser "B"
        |= prizeParser


buttonParser : String -> Parser Button
buttonParser name =
    Parser.succeed Button
        |. Parser.symbol ("Button " ++ name ++ ": X+")
        |= int64Parser
        |. Parser.symbol ", Y+"
        |= int64Parser
        |. Parser.spaces


prizeParser : Parser Prize
prizeParser =
    Parser.succeed Prize
        |. Parser.symbol "Prize: X="
        |= int64Parser
        |. Parser.symbol ", Y="
        |= int64Parser
        |. Parser.spaces


int64Parser : Parser BigInt
int64Parser =
    Parser.int |> Parser.map BigInt.fromInt


part1 : List Machine -> Int
part1 list =
    list
        |> List.map optimize
        |> List.sum


optimize : Machine -> Int
optimize machine =
    let
        a : Maybe Int
        a =
            ratio
                (BigInt.sub (BigInt.mul machine.b.dx machine.prize.y) (BigInt.mul machine.b.dy machine.prize.x))
                (BigInt.sub (BigInt.mul machine.b.dx machine.a.dy) (BigInt.mul machine.b.dy machine.a.dx))

        b : Maybe Int
        b =
            ratio
                (BigInt.sub (BigInt.mul machine.a.dx machine.prize.y) (BigInt.mul machine.a.dy machine.prize.x))
                (BigInt.sub (BigInt.mul machine.a.dx machine.b.dy) (BigInt.mul machine.a.dy machine.b.dx))

        ratio : BigInt -> BigInt -> Maybe Int
        ratio n d =
            BigInt.divmod n d
                |> Maybe.andThen
                    (\( res, mod ) ->
                        if mod == BigInt.fromInt 0 then
                            res
                                |> BigInt.toString
                                |> String.toInt

                        else
                            Nothing
                    )
    in
    case ( a, b ) of
        ( Just ia, Just ib ) ->
            ia * 3 + ib

        _ ->
            0


part2 : List Machine -> Int
part2 list =
    list
        |> List.map
            (\machine ->
                optimize
                    { machine
                        | prize =
                            { x = BigInt.add machine.prize.x (BigInt.fromInt 10000000000000)
                            , y = BigInt.add machine.prize.y (BigInt.fromInt 10000000000000)
                            }
                    }
            )
        |> List.sum
