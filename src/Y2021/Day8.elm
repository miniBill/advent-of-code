module Y2021.Day8 exposing (parser, process, processGold)

import Dict exposing (Dict)
import List.Extra
import Parser exposing ((|.), (|=), Parser)
import Set exposing (Set)


type alias Item =
    ( List Signal, List Signal )


type alias Signal =
    ( String, Set Char )


parser : Parser Item
parser =
    Parser.succeed Tuple.pair
        |= signalListParser
        |. Parser.symbol "| "
        |= signalListParser


signalListParser : Parser (List Signal)
signalListParser =
    Parser.sequence
        { start = ""
        , end = ""
        , separator = " "
        , spaces = Parser.succeed ()
        , item = signalParser
        , trailing = Parser.Optional
        }


signalParser : Parser Signal
signalParser =
    Parser.map
        (\s ->
            let
                set =
                    Set.fromList <| String.toList s
            in
            ( String.fromList <| Set.toList set, set )
        )
        (Parser.getChompedString <|
            Parser.succeed ()
                |. Parser.chompIf Char.isAlpha
                |. Parser.chompWhile Char.isAlpha
        )


type alias StateRecord =
    { segments : Dict Char Char
    , numbers : Dict Int Signal
    , log : List String
    }


initialState : StateRecord
initialState =
    { segments = Dict.empty
    , numbers = Dict.empty
    , log = []
    }


getNumber : Int -> Monad Signal
getNumber i =
    Monad <|
        \s ->
            case Dict.get i s.numbers of
                Nothing ->
                    errorInner ("Cannot find number " ++ String.fromInt i) s

                n ->
                    ( n, s )


getSegment : Char -> Monad Char
getSegment segment =
    Monad <| \s -> ( Dict.get segment s.segments, s )


gotSegmentFromSingleton : Char -> Set Char -> Monad ()
gotSegmentFromSingleton segment set =
    case Set.toList set of
        [ x ] ->
            Monad <| \s -> ( Just (), { s | segments = Dict.insert segment x s.segments } )

        _ ->
            error ("Called gotSegmentFromSingleton " ++ String.fromChar segment ++ " with input " ++ String.fromList (Set.toList set))


process : List Item -> { output : String, log : String }
process lines =
    let
        ( logs, outputs ) =
            lines
                |> List.map
                    (\item ->
                        let
                            p =
                                processItem item
                        in
                        ( p.log, p.output )
                    )
                |> List.unzip
    in
    { log = String.join "\n" logs
    , output = String.join "\n" outputs
    }


processItem : Item -> { log : String, output : String }
processItem ( input, output ) =
    let
        findNumber number condition =
            case List.Extra.find condition input of
                Nothing ->
                    error <| "Could not find number " ++ String.fromInt number

                Just signal ->
                    Monad <| \s -> ( Just (), { s | numbers = Dict.insert number signal s.numbers } )

        (Monad monad) =
            log ("Parsed as " ++ itemToString ( input, output ))
                |> andThen_ (findNumber 1 (\( _, ss ) -> Set.size ss == 2))
                |> andThen_ (findNumber 4 (\( _, ss ) -> Set.size ss == 4))
                |> andThen_ (findNumber 7 (\( _, ss ) -> Set.size ss == 3))
                |> andThen_ (findNumber 8 (\( _, ss ) -> Set.size ss == 7))
                |> andThen_
                    (map2 (\( _, seven ) ( _, one ) -> Set.diff seven one)
                        (getNumber 7)
                        (getNumber 1)
                        |> andThen (gotSegmentFromSingleton 'a')
                    )
                |> andThen_
                    (map2 Tuple.pair
                        (getNumber 4)
                        (getSegment 'a')
                        |> andThen
                            (\( ( _, four ), a ) ->
                                findNumber 9 (\( _, ss ) -> Set.size ss == 6 && Set.isEmpty (Set.diff (Set.insert a four) ss))
                            )
                    )
                |> andThen_
                    (map3 (\( _, nine ) ( _, four ) a -> Set.remove a <| Set.diff nine four)
                        (getNumber 9)
                        (getNumber 4)
                        (getSegment 'a')
                        |> andThen (gotSegmentFromSingleton 'g')
                    )
                |> andThen_
                    (logDict "numbers" String.fromInt Tuple.first .numbers)
                |> andThen_
                    (logDict "segments" String.fromChar String.fromChar .segments)

        ( fv, fs ) =
            monad initialState
    in
    { log = String.join "\n" <| List.reverse fs.log
    , output = Debug.toString fv
    }


logDict label keyLog valueLog dict =
    get
        |> andThen
            (\s ->
                log <|
                    String.join "\n" <|
                        (label ++ ":")
                            :: List.map
                                (\( d, n ) -> "  " ++ keyLog d ++ ": " ++ valueLog n)
                                (Dict.toList <| dict s)
            )


itemToString : ( List Signal, List Signal ) -> String
itemToString ( l, r ) =
    signalListToString l ++ "\n  " ++ signalListToString r


signalListToString : List Signal -> String
signalListToString =
    List.map signalToString >> String.join " "


signalToString : Signal -> String
signalToString =
    Tuple.first


processGold : List Item -> { output : String, log : String }
processGold _ =
    { output = ""
    , log = ""
    }



-- Eh, shuckit I'm writing a Monad


type Monad a
    = Monad (StateRecord -> ( Maybe a, StateRecord ))


andThen : (a -> Monad b) -> Monad a -> Monad b
andThen f (Monad i) =
    Monad <|
        \s ->
            let
                ( v, s_ ) =
                    i s
            in
            case v of
                Nothing ->
                    ( Nothing, s_ )

                Just w ->
                    let
                        (Monad fm) =
                            f w
                    in
                    fm s_


andThen_ : Monad b -> Monad a -> Monad b
andThen_ =
    andThen << always


pure : a -> Monad a
pure x =
    Monad <| \s -> ( Just x, s )


map : (a -> b) -> Monad a -> Monad b
map f x =
    x |> andThen (\xv -> pure <| f xv)


map2 : (a -> b -> c) -> Monad a -> Monad b -> Monad c
map2 f x y =
    x |> andThen (\xv -> map (f xv) y)


map3 : (a -> b -> c -> d) -> Monad a -> Monad b -> Monad c -> Monad d
map3 f x y z =
    x |> andThen (\xv -> y |> andThen (\yv -> map (f xv yv) z))


get : Monad StateRecord
get =
    Monad (\s -> ( Just s, s ))


log : String -> Monad ()
log line =
    Monad <| \s -> ( Just (), { s | log = line :: s.log } )


error : String -> Monad a
error err =
    Monad <| errorInner err


errorInner : String -> StateRecord -> ( Maybe a, StateRecord )
errorInner err s =
    ( Nothing
    , { s | log = err :: s.log }
    )
