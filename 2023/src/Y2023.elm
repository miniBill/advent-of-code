module Y2023 exposing (solve)

import Day exposing (Day(..))
import Star exposing (Star(..))
import Y2023.Day1 as Day1
import Y2023.Day2 as Day2


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

        _ ->
            \_ -> Err <| "Day " ++ String.fromInt (Day.toInt day) ++ ", " ++ Star.toString star ++ " star not implemented yet"
