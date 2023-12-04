module Main exposing (main)

import Browser
import Dict
import Element exposing (Attribute, Element, column, el, text, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import File exposing (File)
import File.Select
import List.Extra
import Parser exposing (Parser, Problem(..))
import Task
import Y2020.Day1
import Y2020.Day2
import Y2020.Day3
import Y2020.Day4
import Y2020.Day5
import Y2020.Day6
import Y2020.Day7
import Y2021.Day1
import Y2021.Day10
import Y2021.Day2
import Y2021.Day3
import Y2021.Day4
import Y2021.Day5
import Y2021.Day6
import Y2021.Day7
import Y2021.Day8
import Y2021.Day9


type alias Flags =
    {}


type alias Model =
    { input : String
    , year : Int
    , index : Int
    }


type Msg
    = IndexChosen Int Int
    | PickFile
    | GotFile File
    | ReadFile String


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = Element.layout [] << view
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd msg )
init _ =
    ( { input = ""
      , year = -1
      , index = -1
      }
    , Cmd.none
    )


view : Model -> Element Msg
view model =
    let
        years =
            [ ( 2020
              , [ dayISplit Y2020.Day1.parser Y2020.Day1.process Y2020.Day1.processGold
                , dayISplit Y2020.Day2.parser Y2020.Day2.process Y2020.Day2.processGold
                , dayISplit Y2020.Day3.parser Y2020.Day3.process Y2020.Day3.processGold
                , dayISplit Y2020.Day4.parser Y2020.Day4.process Y2020.Day4.processGold
                , dayISplit Y2020.Day5.parser Y2020.Day5.process Y2020.Day5.processGold
                , dayISplit Y2020.Day6.parser Y2020.Day6.process Y2020.Day6.processGold
                , dayISplit Y2020.Day7.parser Y2020.Day7.process Y2020.Day7.processGold
                ]
              )
            , ( 2021
              , [ dayISplit Y2021.Day1.parser Y2021.Day1.process Y2021.Day1.processGold
                , dayISplit Y2021.Day2.parser Y2021.Day2.process Y2021.Day2.processGold
                , dayISplit Y2021.Day3.parser Y2021.Day3.process Y2021.Day3.processGold
                , dayS Y2021.Day4.parser Y2021.Day4.process Y2021.Day4.processGold
                , daySSplit Y2021.Day5.parser Y2021.Day5.process Y2021.Day5.processGold
                , dayS Y2021.Day6.parser Y2021.Day6.process Y2021.Day6.processGold
                , dayS Y2021.Day7.parser Y2021.Day7.process Y2021.Day7.processGold
                , daySplit Y2021.Day8.parser Y2021.Day8.process Y2021.Day8.processGold
                , daySplit Y2021.Day9.parser Y2021.Day9.process Y2021.Day9.processGold
                , daySplit Y2021.Day10.parser Y2021.Day10.process Y2021.Day10.processGold
                ]
              )
            ]
                |> List.map (Tuple.mapSecond List.concat)
                |> Dict.fromList

        dayPickers =
            years
                |> Dict.toList
                |> List.map
                    (\( year, processes ) ->
                        List.range 0 (List.length processes - 1)
                            |> List.map (dayPicker year)
                            |> (::) (text <| String.fromInt year ++ ": ")
                            |> wrappedRow [ spacing ]
                    )
                |> column [ spacing ]

        dayPicker year index =
            let
                label =
                    "Day "
                        ++ String.fromInt (index // 2 + 1)
                        ++ (if modBy 2 index == 0 then
                                ""

                            else
                                "*"
                           )
            in
            if year == model.year && index == model.index then
                el
                    [ Border.width 1
                    , Border.rounded rythm
                    , padding
                    , Background.color <| Element.rgb 0.8 0.8 0.8
                    ]
                    (text label)

            else
                button []
                    { onPress = IndexChosen year index
                    , label = label
                    }

        maybeProcess =
            years
                |> Dict.get model.year
                |> Maybe.andThen
                    (List.drop model.index
                        >> List.head
                    )
    in
    column [ spacing, padding ]
        [ dayPickers
        , button []
            { onPress = PickFile
            , label = "Pick File"
            }
        , case ( String.isEmpty model.input, maybeProcess ) of
            ( _, Nothing ) ->
                text "Pick a day"

            ( True, _ ) ->
                text "Load a file"

            ( False, Just process ) ->
                let
                    { output, log } =
                        process model.input
                in
                column [ spacing ] <|
                    List.concat
                        [ if List.isEmpty log then
                            []

                          else
                            [ text "Log"
                            , el
                                [ Border.width 1
                                , padding
                                , Font.family [ Font.monospace ]
                                ]
                                (text <| String.join "\n" log)
                            ]
                        , [ text "Output"
                          , el
                                [ Border.width 1
                                , padding
                                , Font.family [ Font.monospace ]
                                ]
                                (text output)
                          ]
                        ]
        ]


type alias Day =
    String -> DayOutput


type alias DayOutput =
    { output : String
    , log : List String
    }


dayISplit : Parser a -> (List a -> Int) -> (List a -> Int) -> List Day
dayISplit parser first second =
    daySSplit parser (first >> String.fromInt) (second >> String.fromInt)


daySSplit : Parser a -> (List a -> String) -> (List a -> String) -> List Day
daySSplit parser =
    let
        listParser =
            Parser.sequence
                { start = ""
                , end = ""
                , separator = "\n"
                , item = parser
                , spaces = Parser.succeed ()
                , trailing = Parser.Optional
                }
    in
    dayS listParser


daySplit : Parser a -> (List a -> DayOutput) -> (List a -> DayOutput) -> List Day
daySplit parser =
    let
        listParser =
            Parser.sequence
                { start = ""
                , end = ""
                , separator = "\n"
                , item = parser
                , spaces = Parser.succeed ()
                , trailing = Parser.Optional
                }
    in
    day listParser


dayS : Parser a -> (a -> String) -> (a -> String) -> List Day
dayS parser first second =
    [ withParserS parser first
    , withParserS parser second
    ]


day : Parser a -> (a -> DayOutput) -> (a -> DayOutput) -> List Day
day parser first second =
    [ withParser parser first
    , withParser parser second
    ]


withParserS : Parser a -> (a -> String) -> Day
withParserS parser f =
    withParser parser <| \input ->
    { output = f input
    , log = []
    }


withParser : Parser a -> (a -> DayOutput) -> Day
withParser parser f source =
    let
        parsed =
            source
                |> String.trimRight
                |> Parser.run parser
    in
    case parsed of
        Ok o ->
            f o

        Err err ->
            let
                errorString =
                    deadEndsToString err source
            in
            { log = [ "Failed to parse line:\n\n" ++ errorString ]
            , output = ""
            }


deadEndsToString : List Parser.DeadEnd -> String -> String
deadEndsToString err source =
    let
        lines =
            String.split "\n" source

        getLine line =
            lines
                |> List.drop (line - 1)
                |> List.head
                |> Maybe.withDefault ""

        indentedLine line =
            "    " ++ getLine line ++ "\n"

        marker col =
            "\n" ++ String.repeat (4 + col - 1) " " ++ "^-- "
    in
    err
        |> List.Extra.gatherEqualsBy (\e -> { row = e.row, col = e.col })
        |> List.map
            (\( h, t ) ->
                let
                    ( expected, unexpected ) =
                        formatProblems (h :: t)

                    combine labelOne labelMore pieces =
                        case List.reverse pieces of
                            [] ->
                                ""

                            [ es ] ->
                                marker h.col ++ labelOne ++ " " ++ es

                            eh :: et ->
                                marker h.col ++ labelMore ++ " " ++ String.join ", " (List.reverse et) ++ " or " ++ eh

                    expectedString =
                        combine "expected" "expected one of" expected

                    unexpectedString =
                        combine "unexpected" "unexpected" unexpected
                in
                indentedLine h.row ++ expectedString ++ unexpectedString
            )
        |> String.join "\n\n"


formatProblems : List { a | problem : Parser.Problem } -> ( List String, List String )
formatProblems problems =
    problems
        |> List.foldr
            (\{ problem } ( exp, unexp ) ->
                case problem of
                    Expecting s ->
                        ( escape s :: exp, unexp )

                    ExpectingInt ->
                        ( "an integer" :: exp, unexp )

                    ExpectingHex ->
                        ( "an hex number" :: exp, unexp )

                    ExpectingOctal ->
                        ( "an octal number" :: exp, unexp )

                    ExpectingBinary ->
                        ( "a binary number" :: exp, unexp )

                    ExpectingFloat ->
                        ( "a float" :: exp, unexp )

                    ExpectingNumber ->
                        ( "a number" :: exp, unexp )

                    ExpectingVariable ->
                        ( "a variable" :: exp, unexp )

                    ExpectingSymbol s ->
                        ( ("\"" ++ escape s ++ "\"") :: exp, unexp )

                    ExpectingKeyword k ->
                        ( ("\"" ++ escape k ++ "\"") :: exp, unexp )

                    ExpectingEnd ->
                        ( "the end of the input" :: exp, unexp )

                    UnexpectedChar ->
                        ( exp, "character" :: unexp )

                    Problem p ->
                        ( exp, ("problem " ++ p) :: unexp )

                    BadRepeat ->
                        ( exp, "repetition" :: unexp )
            )
            ( [], [] )


escape : String -> String
escape s =
    s |> String.replace "\n" "\\n" |> String.replace "\t" "\\t"


button : List (Attribute msg) -> { onPress : msg, label : String } -> Element msg
button attrs { onPress, label } =
    Input.button
        ([ Border.width 1
         , Border.rounded rythm
         , padding
         ]
            ++ attrs
        )
        { onPress = Just onPress
        , label = text label
        }


padding : Attribute msg
padding =
    Element.padding rythm


spacing : Attribute msg
spacing =
    Element.spacing rythm


rythm : number
rythm =
    10


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        IndexChosen year index ->
            ( { model
                | year = year
                , index = index
              }
            , Cmd.none
            )

        GotFile file ->
            ( model
            , File.toString file
                |> Task.perform ReadFile
            )

        ReadFile input ->
            ( { model | input = input }, Cmd.none )

        PickFile ->
            ( model, File.Select.file [ "text/plain" ] GotFile )


subscriptions : model -> Sub msg
subscriptions _ =
    Sub.none
