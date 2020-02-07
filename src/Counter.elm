module Counter exposing (Model, Msg, init, update, view)

import Element exposing (el, padding, row, spacing, text)
import Element.Font as Font
import Element.Input exposing (button)


type alias Model =
    Int


type Msg
    = Plus
    | Minus


init : Model
init =
    0


update : Msg -> Model -> Model
update msg model =
    case msg of
        Plus ->
            model + 1

        Minus ->
            max 0 <| model - 1


view : String -> Model -> Element.Element Msg
view title model =
    row
        [ padding 10
        , spacing 30
        ]
        [ el [ Font.size 60 ] (text title)
        , button [ Font.size 60 ] { onPress = Just Minus, label = text "-" }
        , el [ Font.size 60 ]
            (text <| String.fromInt model)
        , button [ Font.size 60 ] { onPress = Just Plus, label = text "+" }
        ]
