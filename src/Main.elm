module Main exposing (main)

import Browser
import Day1
import Element exposing (Attribute, Element, column, el, text, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import File exposing (File)
import File.Select
import Task


type alias Flags =
    {}


type alias Model =
    { input : String
    , day : Int
    }


type Msg
    = DayChosen Int
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
    ( { input = "", day = -1 }
    , Cmd.none
    )


view : Model -> Element Msg
view model =
    let
        maxDay =
            3

        dayPickers =
            List.range 2 maxDay
                |> List.map dayPicker
                |> wrappedRow [ spacing ]

        dayPicker day =
            let
                label =
                    "Day "
                        ++ String.fromInt (day // 2)
                        ++ (if modBy 2 day == 0 then
                                ""

                            else
                                "*"
                           )
            in
            if day == model.day then
                el
                    [ Border.width 1
                    , Border.rounded rythm
                    , padding
                    , Background.color <| Element.rgb 0.8 0.8 0.8
                    ]
                    (text label)

            else
                button []
                    { onPress = DayChosen day
                    , label = label
                    }

        maybeProcess =
            case model.day of
                2 ->
                    Just <| Day1.process

                3 ->
                    Just <| Day1.processGold

                _ ->
                    Nothing
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
        DayChosen i ->
            ( { model | day = i }, Cmd.none )

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
