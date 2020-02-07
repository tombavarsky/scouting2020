module Teleop exposing (Model, Msg, boolToText, createButton, init, subscriptions, update, view, yophyTophy)

import Colors exposing (blue, purple, sky, white)
import Counter
import Element exposing (centerX, centerY, column, el, padding, spacing, text)
import Element.Background as Background
import Element.Border exposing (rounded)
import Element.Font as Font exposing (center)
import Element.Input exposing (button)


type Msg
    = LowLevel Counter.Msg
    | HighLevel Counter.Msg
    | Missed Counter.Msg
    | ColorRoulette
    | SpunRoulette


type alias Model =
    { lowlevel : Counter.Model
    , highlevel : Counter.Model
    , missed : Counter.Model
    , colorRoulette : Bool
    , spunRoulette : Bool
    }


init : Model
init =
    Model Counter.init Counter.init Counter.init False False


createButton : Msg -> String -> Element.Element Msg
createButton msg name =
    button
        [ Font.color white
        , Font.size 60
        , Font.glow blue 5
        , rounded 10
        , Font.family
            [ Font.external
                { name = "Open Sans"
                , url = "https://fonts.googleapis.com/css?family=Open+Sans:700i&display=swap"
                }
            ]
        , Background.color purple
        , center
        , centerX
        , centerY
        ]
        { onPress = Just msg, label = text name }


update : Msg -> Model -> Model
update msg model =
    let
        counterUpdate : Counter.Msg -> Counter.Model -> Counter.Model
        counterUpdate =
            Counter.update
    in
    case msg of
        LowLevel count ->
            { model | lowlevel = counterUpdate count model.lowlevel }

        HighLevel count ->
            { model | highlevel = counterUpdate count model.highlevel }

        ColorRoulette ->
            { model | colorRoulette = not model.colorRoulette }

        SpunRoulette ->
            { model | spunRoulette = not model.spunRoulette }

        Missed count ->
            { model | missed = counterUpdate count model.missed }


view : Model -> Element.Element Msg
view model =
    column
        [ Background.color sky
        , padding 50
        , spacing 20
        , rounded 20
        , centerX
        , centerY
        ]
        [ el yophyTophy
            (text "spun to\ncorrect color?")
        , createButton ColorRoulette <| boolToText model.colorRoulette
        , el yophyTophy
            (text "spun cycles 3-5?")
        , createButton SpunRoulette <| boolToText model.spunRoulette
        , Element.map LowLevel <| Counter.view "low Level:" model.lowlevel
        , Element.map HighLevel <| Counter.view "high Level:" model.highlevel
        , Element.map Missed <| Counter.view "missed:" model.missed
        ]


boolToText : Bool -> String
boolToText bool =
    if bool then
        "Yes"

    else
        "No"


yophyTophy : List (Element.Attribute Msg)
yophyTophy =
    [ padding 10
    , spacing 5
    , centerX
    , centerY
    , Font.size 60
    ]


subscriptions : Sub Msg
subscriptions =
    Sub.none
