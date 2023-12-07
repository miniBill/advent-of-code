module Y2023 exposing (solve)

import Day exposing (Day(..))
import Star exposing (Star(..))
import Y2023.Day1 as Day1
import Y2023.Day2 as Day2
import Y2023.Day3 as Day3
import Y2023.Day4 as Day4


solve : Day -> Star -> String -> Result String String
solve day star =
    case ( day, star ) of
        ( Day1, Silver ) ->
            Day1.silver

        ( Day1, Gold ) ->
            Day1.gold

        ( Day2, Silver ) ->
            Day2.silver

        ( Day2, Gold ) ->
            Day2.gold

        ( Day3, Silver ) ->
            Day3.silver

        ( Day3, Gold ) ->
            Day3.gold

        ( Day4, Silver ) ->
            Day4.silver

        ( Day4, Gold ) ->
            Day4.gold

        _ ->
            \_ -> Err <| "Day " ++ String.fromInt (Day.toInt day) ++ ", " ++ Star.toString star ++ " star not implemented yet"
