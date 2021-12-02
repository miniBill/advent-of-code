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
import Result.Extra
import Task
import Y2020.Day1
import Y2020.Day2
import Y2020.Day3
import Y2020.Day4
import Y2021.Day1
import Y2021.Day2


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
              , [ dayI Y2020.Day1.parser Y2020.Day1.process Y2020.Day1.processGold
                , dayI Y2020.Day2.parser Y2020.Day2.process Y2020.Day2.processGold
                , dayI Y2020.Day3.parser Y2020.Day3.process Y2020.Day3.processGold
                , dayI Y2020.Day4.parser Y2020.Day4.process Y2020.Day4.processGold
                ]
              )
            , ( 2021
              , [ dayI Y2021.Day1.parser Y2021.Day1.process Y2021.Day1.processGold
                , dayI Y2021.Day2.parser Y2021.Day2.process Y2021.Day2.processGold
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
                el [ Font.family [ Font.monospace ] ]
                    (text <| process <| String.split "\n" model.input)
        ]


dayI : Parser a -> (List a -> Int) -> (List a -> Int) -> List (List String -> String)
dayI parser first second =
    dayS parser (first >> String.fromInt) (second >> String.fromInt)


dayS : Parser a -> (List a -> String) -> (List a -> String) -> List (List String -> String)
dayS parser first second =
    [ withParser parser first
    , withParser parser second
    ]


withParser : Parser a -> (List a -> String) -> List String -> String
withParser parser f lines =
    let
        parsed =
            lines
                |> List.Extra.dropWhileRight String.isEmpty
                |> Result.Extra.combineMap
                    (\line ->
                        Result.mapError
                            (Tuple.pair line)
                            (Parser.run parser line)
                    )
    in
    case parsed of
        Ok o ->
            f o

        Err ( line, err ) ->
            let
                errorString =
                    deadEndsToString err line
            in
            "Failed to parse line:\n\n" ++ errorString


deadEndsToString : List Parser.DeadEnd -> String -> String
deadEndsToString err line =
    let
        indentedLine =
            "    " ++ line ++ "\n"

        marker col =
            "\n" ++ String.repeat (4 + col - 1) " " ++ "^-- "
    in
    err
        |> List.Extra.gatherEqualsBy .col
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
                indentedLine ++ expectedString ++ unexpectedString
            )
        |> String.join "\n\n"


formatProblems : List { a | problem : Parser.Problem } -> ( List String, List String )
formatProblems problems =
    problems
        |> List.foldr
            (\{ problem } ( exp, unexp ) ->
                case problem of
                    Expecting s ->
                        ( s :: exp, unexp )

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
                        ( ("\"" ++ s ++ "\"") :: exp, unexp )

                    ExpectingKeyword k ->
                        ( ("\"" ++ k ++ "\"") :: exp, unexp )

                    ExpectingEnd ->
                        ( "the end of the line" :: exp, unexp )

                    UnexpectedChar ->
                        ( exp, "character" :: unexp )

                    Problem p ->
                        ( exp, ("problem " ++ p) :: unexp )

                    BadRepeat ->
                        ( exp, "repetition" :: unexp )
            )
            ( [], [] )


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
