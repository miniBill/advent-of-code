module Test2023.Day1 exposing (suite)

import Expect
import Test exposing (Test, test)
import Y2023.Day1


goldInput : String
goldInput =
    """two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen"""


suite : Test
suite =
    test "Gold works on example input" <|
        \_ ->
            Y2023.Day1.gold goldInput
                |> Expect.equal (Ok "281")
