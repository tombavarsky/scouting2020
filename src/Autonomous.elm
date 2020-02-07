module Autonomous exposing (Model, Msg, init, subscriptions, update, view)

import Colors exposing (blue, purple, sky, white)
import Counter
import Element exposing (centerX, centerY, column, el, padding, spacing, text)
import Element.Background as Background
import Element.Border as Border exposing (rounded)
import Element.Font as Font exposing (center)
import Element.Input as Input exposing (button, radio)


type Msg
    = Moved
    | LowLevel Counter.Msg
    | HighLevel Counter.Msg
    | Missed Counter.Msg
    | TrenchCollection Counter.Msg
    | EnemyTrenchCollection Counter.Msg
    | RendezvousCollection Counter.Msg
    | BallsAmount BallsInitAmount


type alias Model =
    { ballsAmount : BallsInitAmount
    , moved : Bool
    , lowlevel : Counter.Model
    , highlevel : Counter.Model
    , missed : Counter.Model
    , trenchCollection : Counter.Model
    , enemyTrenchCollection : Counter.Model
    , rendezvousCollection : Counter.Model
    }


type BallsInitAmount
    = NoBalls
    | OneBall
    | TwoBalls
    | ThreeBalls


createButton : Msg -> String -> Element.Element Msg
createButton msg name =
    button
        [ Font.color white
        , Font.size 60
        , Font.glow blue 5
        , Border.rounded 4
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
        [ radio
            [ padding 10
            , spacing 20
            , Font.size 60
            ]
            { onChange = BallsAmount
            , selected = Just model.ballsAmount
            , label = Input.labelAbove [ Font.size 60, padding 10, spacing 20 ] (text "started with:")
            , options =
                [ Input.option NoBalls (text "0 balls")
                , Input.option OneBall (text "1 ball")
                , Input.option TwoBalls (text "2 balls")
                , Input.option ThreeBalls (text "3 balls")
                ]
            }
        , createButton Moved <|
            if model.moved then
                "moved."

            else
                "moved?"
        , Element.map LowLevel <| Counter.view "low Level:" model.lowlevel
        , Element.map HighLevel <| Counter.view "high Level:" model.highlevel
        , Element.map Missed <| Counter.view "missed:" model.missed
        , el [ Font.size 60, padding 10 ] (text "Collected from:")
        , Element.map TrenchCollection <| Counter.view "their trench:" model.trenchCollection
        , Element.map EnemyTrenchCollection <| Counter.view "enemy's trench:" model.enemyTrenchCollection
        , Element.map RendezvousCollection <| Counter.view "rendezvous:" model.rendezvousCollection
        ]


init : Model
init =
    Model NoBalls False Counter.init Counter.init Counter.init Counter.init Counter.init <| Counter.init


update : Msg -> Model -> Model
update msg model =
    let
        counterUpdate : Counter.Msg -> Counter.Model -> Counter.Model
        counterUpdate =
            Counter.update
    in
    case msg of
        Moved ->
            { model | moved = not model.moved }

        LowLevel count ->
            { model | lowlevel = counterUpdate count model.lowlevel }

        HighLevel count ->
            { model | highlevel = counterUpdate count model.highlevel }

        Missed count ->
            { model | missed = counterUpdate count model.missed }

        TrenchCollection count ->
            { model | trenchCollection = counterUpdate count model.trenchCollection }

        BallsAmount ballsAmount ->
            { model | ballsAmount = ballsAmount }

        EnemyTrenchCollection count ->
            { model | enemyTrenchCollection = counterUpdate count model.enemyTrenchCollection }

        RendezvousCollection count ->
            { model | rendezvousCollection = counterUpdate count model.rendezvousCollection }


subscriptions : Sub Msg
subscriptions =
    Sub.none
