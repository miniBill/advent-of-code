module Main exposing (main)

import Browser
import Day1
import Day2
import Element exposing (Attribute, Element, column, el, text, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import File exposing (File)
import File.Select
import Parser exposing (Parser)
import Result.Extra
import Task


type alias Flags =
    {}


type alias Model =
    { input : String
    , index : Int
    }


type Msg
    = IndexChosen Int
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
    ( { input = "", index = -1 }
    , Cmd.none
    )


view : Model -> Element Msg
view model =
    let
        processes =
            [ day Day1.parser Day1.process Day1.processGold
            , day Day2.parser Day2.process Day2.processGold
            ]
                |> List.concat

        dayPickers =
            List.range 0 (List.length processes - 1)
                |> List.map dayPicker
                |> wrappedRow [ spacing ]

        dayPicker index =
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
            if index == model.index then
                el
                    [ Border.width 1
                    , Border.rounded rythm
                    , padding
                    , Background.color <| Element.rgb 0.8 0.8 0.8
                    ]
                    (text label)

            else
                button []
                    { onPress = IndexChosen index
                    , label = label
                    }

        maybeProcess =
            processes
                |> List.drop model.index
                |> List.head
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


day : Parser a -> (List a -> Int) -> (List a -> Int) -> List (List String -> String)
day parser first second =
    [ withParser parser first
    , withParser parser second
    ]


withParser : Parser a -> (List a -> Int) -> List String -> String
withParser parser f lines =
    let
        parsed =
            lines
                |> List.filter (not << String.isEmpty)
                |> Result.Extra.combineMap
                    (\line ->
                        Result.mapError
                            (Tuple.pair line)
                            (Parser.run parser line)
                    )
    in
    case parsed of
        Ok o ->
            String.fromInt <| f o

        Err ( line, err ) ->
            "Failed to parse line \"" ++ line ++ "\"" ++ Debug.toString err


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
        IndexChosen i ->
            ( { model | index = i }, Cmd.none )

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
