module Y2021.Day8 exposing (parser, process, processGold)

import Dict exposing (Dict)
import List.Extra
import Maybe.Extra
import Parser exposing ((|.), (|=), Parser)
import Set exposing (Set)


type alias Item =
    ( List Signal, { one : Signal, two : Signal, three : Signal, four : Signal } )


type alias Signal =
    ( String, Set Char )


parser : Parser Item
parser =
    Parser.succeed Tuple.pair
        |= signalListParser
        |. Parser.symbol "| "
        |= Parser.andThen
            (\l ->
                case l of
                    [ one, two, three, four ] ->
                        Parser.succeed { one = one, two = two, three = three, four = four }

                    _ ->
                        Parser.problem "Too many or two few"
            )
            signalListParser


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
    { numbers : Dict String Int
    , log : List String
    }


initialState : StateRecord
initialState =
    { numbers = Dict.empty
    , log = []
    }


gotSegmentFromSingleton : Char -> Set Char -> (Char -> Monad a) -> Monad a
gotSegmentFromSingleton segment set f =
    case Set.toList set of
        [ x ] ->
            andThen f <|
                log 1 ("Got segment " ++ String.fromChar segment ++ " from singleton: " ++ String.fromChar x) <| \_ ->
                pure x

        _ ->
            error ("Called gotSegmentFromSingleton " ++ String.fromChar segment ++ " with input " ++ String.fromList (Set.toList set))


process : List Item -> { output : String, log : String }
process lines =
    let
        isUnique x =
            x == 1 || x == 4 || x == 7 || x == 8

        ( logs, outputs ) =
            lines
                |> List.map
                    (\item ->
                        let
                            p =
                                processItem item
                        in
                        ( p.log
                        , Maybe.map
                            (\{ one, two, three, four } ->
                                List.Extra.count isUnique [ one, two, three, four ]
                            )
                            p.output
                        )
                    )
                |> List.unzip
    in
    { log = String.join "\n" <| List.Extra.filterNot String.isEmpty logs
    , output =
        case Maybe.Extra.combine outputs of
            Nothing ->
                "Error"

            Just outs ->
                String.fromInt <| List.sum outs
    }


isSubsetof : Set comparable -> Set comparable -> Bool
isSubsetof big small =
    Set.isEmpty <| Set.diff small big


processItem : Item -> { log : String, output : Maybe { one : Int, two : Int, three : Int, four : Int } }
processItem ( input, output ) =
    let
        findNumber number condition f =
            andThen f <|
                case List.filter (\( _, ss ) -> condition ss) input of
                    [] ->
                        error <| "Could not find number " ++ String.fromInt number

                    [ ( signalString, signalSet ) ] ->
                        log 1 ("Found number " ++ String.fromInt number ++ ": " ++ signalString) <| \_ s ->
                        ( Just signalSet, { s | numbers = Dict.insert signalString number s.numbers } )

                    candidates ->
                        error <| "Too many candidates (" ++ String.fromInt (List.length candidates) ++ ") for number " ++ String.fromInt number

        monad =
            log 0 ("Parsed as " ++ itemToString ( input, output )) <| \_ ->
            findNumber 1 (\ss -> Set.size ss == 2) <| \one ->
            findNumber 4 (\ss -> Set.size ss == 4) <| \four ->
            findNumber 7 (\ss -> Set.size ss == 3) <| \seven ->
            findNumber 8 (\ss -> Set.size ss == 7) <| \_ ->
            gotSegmentFromSingleton 'a' (Set.diff seven one) <| \a ->
            findNumber 9 (\ss -> Set.size ss == 6 && isSubsetof ss (Set.insert a four)) <| \nine ->
            gotSegmentFromSingleton 'g' (Set.remove a <| Set.diff nine four) <| \g ->
            findNumber 3 (\ss -> Set.size ss == 5 && isSubsetof ss (Set.insert a <| Set.insert g one)) <| \three ->
            gotSegmentFromSingleton 'd' (Set.remove a <| Set.remove g <| Set.diff three one) <| \d ->
            gotSegmentFromSingleton 'b' (Set.remove d <| Set.diff four one) <| \_ ->
            findNumber 6 (\ss -> Set.size ss == 6 && not (isSubsetof ss one)) <| \six ->
            findNumber 5 (\ss -> Set.size ss == 5 && isSubsetof six ss) <| \five ->
            findNumber 2 (\ss -> Set.size ss == 5 && ss /= five && ss /= three) <| \_ ->
            findNumber 0 (\ss -> Set.size ss == 6 && ss /= nine && ss /= six) <| \_ ->
            -- gotSegmentFromSingleton 'c' (Set.diff four five) <| \c ->
            -- gotSegmentFromSingleton 'e' (Set.diff two three) <| \e ->
            -- gotSegmentFromSingleton 'f' (Set.intersect one six) <| \f ->
            getNumber output.one <| \outputNumberOne ->
            getNumber output.two <| \outputNumberTwo ->
            getNumber output.three <| \outputNumberThree ->
            getNumber output.four <| \outputNumberFour ->
            pure
                { one = outputNumberOne
                , two = outputNumberTwo
                , three = outputNumberThree
                , four = outputNumberFour
                }

        ( fv, fs ) =
            monad initialState

        finalLog =
            String.join "\n" <| List.reverse fs.log
    in
    { log = always "" finalLog
    , output = fv
    }


getNumber : Signal -> (Int -> Monad a) -> Monad a
getNumber ( ss, _ ) f =
    andThen f <| \s -> ( Dict.get ss s.numbers, s )


itemToString : Item -> String
itemToString ( l, r ) =
    signalListToString l ++ "\n  " ++ signalListToString [ r.one, r.two, r.three, r.four ]


signalListToString : List Signal -> String
signalListToString =
    List.map signalToString >> String.join " "


signalToString : Signal -> String
signalToString =
    Tuple.first


processGold : List Item -> { output : String, log : String }
processGold lines =
    let
        ( logs, outputs ) =
            lines
                |> List.map
                    (\item ->
                        let
                            p =
                                processItem item
                        in
                        ( p.log
                        , Maybe.map
                            (\{ one, two, three, four } ->
                                one * 1000 + two * 100 + three * 10 + four
                            )
                            p.output
                        )
                    )
                |> List.unzip
    in
    { log = String.join "\n" <| List.Extra.filterNot String.isEmpty logs
    , output =
        case Maybe.Extra.combine outputs of
            Nothing ->
                "Error"

            Just outs ->
                String.fromInt <| List.sum outs
    }



-- Eh, shuckit I'm writing a Monad


type alias Monad a =
    StateRecord -> ( Maybe a, StateRecord )


andThen : (a -> Monad b) -> Monad a -> Monad b
andThen f i s =
    let
        ( v, s_ ) =
            i s
    in
    case v of
        Nothing ->
            ( Nothing, s_ )

        Just w ->
            f w s_


pure : a -> Monad a
pure x s =
    ( Just x, s )


log : Int -> String -> (() -> Monad a) -> Monad a
log indent line f =
    andThen f <|
        \s -> ( Just (), { s | log = (String.repeat indent "    " ++ line) :: s.log } )


error : String -> Monad a
error err s =
    ( Nothing
    , { s | log = ("ERR " ++ err) :: s.log }
    )
