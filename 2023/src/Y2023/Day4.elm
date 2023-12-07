module Y2023.Day4 exposing (gold, silver)

import Utils


silver : String -> Result String String
silver =
    Utils.perLineWith (\_ -> Err "TODO") Ok


gold : String -> Result String String
gold =
    Utils.perLineWith (\_ -> Err "TODO") Ok
