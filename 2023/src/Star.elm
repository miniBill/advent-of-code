module Star exposing (Star(..), toString)


type Star
    = Silver
    | Gold


toString : Star -> String
toString star =
    case star of
        Silver ->
            "silver"

        Gold ->
            "gold"
