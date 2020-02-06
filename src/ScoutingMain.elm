module ScoutingMain exposing (Model, Msg, init, update, view)

import Array
import Autonomous
import Browser
import Climbing
import Colors exposing (blue, purple, white)
import Element exposing (centerX, centerY, column, el, fill, height, htmlAttribute, layout, maximum, padding, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font exposing (center)
import Element.Input exposing (button)
import GetMatch
import Html.Attributes exposing (style)
import Result.Extra exposing (merge)
import TeamData
import Teleop


main : Program () Model Msg
main =
    Browser.element
        { init = always ( init, Cmd.none )
        , view = view >> layout [ htmlAttribute <| style "touch-action" "manipulation" ]
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = always Sub.none
        }


type Pages
    = TeamDataPage
    | AutonomousPage
    | TeleopPage
    | ClimbingPage


type Msg
    = TeamDataMsg TeamData.Msg
    | AutonomousDataMsg Autonomous.Msg
    | TeleopDataMsg Teleop.Msg
    | ClimbingDataMsg Climbing.Msg
    | PrevPage
    | NextPage


type PagePosition
    = FirstPage
    | MiddlePage
    | LastPage


type alias Model =
    { teamData : TeamData.Model
    , autonomousData : Autonomous.Model
    , teleopData : Teleop.Model
    , climbingData : Climbing.Model
    , pages : Pages
    }


findColor : String -> Element.Color
findColor alliance =
    if String.contains "Blue" alliance then
        Colors.blue

    else if String.contains "Red" alliance then
        Colors.red

    else
        Colors.yellow


stylishPage : String -> PagePosition -> String -> String -> Element.Element Msg -> Element.Element Msg
stylishPage station position title teamNumber page =
    let
        decoration : Int -> List (Element.Attribute Msg)
        decoration size =
            [ padding 10
            , spacing 10
            , centerX
            , centerY
            , Font.color Colors.veryLightBlue
            , Font.size size
            ]
    in
    column
        [ Background.color <| findColor station
        , spacing 10
        , width fill
        , height fill
        , centerY
        ]
        [ el
            (decoration 20)
            (text <| title)
        , el
            (decoration 15)
            (text <| "\nscouted team: " ++ teamNumber)
        , page
        , case position of
            FirstPage ->
                button
                    buttonStyle
                    { onPress = Just <| NextPage
                    , label = Element.text "Next Page"
                    }

            LastPage ->
                button
                    buttonStyle
                    { onPress = Just <| PrevPage
                    , label = Element.text "Previous Page"
                    }

            MiddlePage ->
                column
                    [ spacing 10
                    , width fill
                    , height fill
                    , centerY
                    ]
                    [ button
                        buttonStyle
                        { onPress = Just <| NextPage
                        , label = Element.text "Next Page"
                        }
                    , button
                        buttonStyle
                        { onPress = Just <| PrevPage
                        , label = Element.text "Previous Page"
                        }
                    ]
        ]


init : Model
init =
    { teamData = TeamData.init <| Array.fromList GetMatch.matches
    , autonomousData = Autonomous.init
    , teleopData = Teleop.init
    , climbingData = Climbing.init
    , pages = TeamDataPage
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        TeamDataMsg teamMsg ->
            { model | teamData = TeamData.update teamMsg model.teamData }

        AutonomousDataMsg autoMsg ->
            { model | autonomousData = Autonomous.update autoMsg model.autonomousData }

        TeleopDataMsg telMsg ->
            { model | teleopData = Teleop.update telMsg model.teleopData }

        ClimbingDataMsg climbMsg ->
            { model | climbingData = Climbing.update climbMsg model.climbingData }

        PrevPage ->
            if model.pages == AutonomousPage then
                { model | pages = TeamDataPage }

            else if model.pages == TeleopPage then
                { model | pages = AutonomousPage }

            else if model.pages == ClimbingPage then
                { model | pages = TeleopPage }

            else
                model

        NextPage ->
            let
                matchError : Result String GetMatch.Match
                matchError =
                    TeamData.getMatch model.teamData

                stationError : Result String Int
                stationError =
                    TeamData.getTeam2 model.teamData

                verifier : Bool
                verifier =
                    (not <| List.member matchError [ Err "No such match", Err "Match number must be a number" ])
                        && stationError
                        /= Err "No station"
                        && (not << String.isEmpty << .scouterName << .teamData) model
                        || List.member model.teamData.scouterName [ "Itamar", "tom", "hadar", "shira" ]
            in
            if model.pages == TeamDataPage && verifier then
                { model | pages = AutonomousPage }

            else if model.pages == AutonomousPage then
                { model | pages = TeleopPage }

            else if model.pages == TeleopPage then
                { model | pages = ClimbingPage }

            else
                model


view : Model -> Element.Element Msg
view model =
    let
        middlePage : String -> Element.Element Msg -> Element.Element Msg
        middlePage name =
            el
                [ Background.color <| findColor (TeamData.stationToString model.teamData.station)
                , padding 105
                , spacing 10
                , width fill
                , height fill
                , centerY
                , centerX
                ]
                << stylishPage
                    (TeamData.stationToString model.teamData.station)
                    MiddlePage
                    name
                    (TeamData.getTeam2 model.teamData
                        |> Result.map String.fromInt
                        |> merge
                    )
    in
    case model.pages of
        TeamDataPage ->
            middlePage
                "Registeration"
                << Element.map TeamDataMsg
            <|
                TeamData.view model.teamData

        AutonomousPage ->
            middlePage
                "Autonomous"
                << Element.map AutonomousDataMsg
            <|
                Autonomous.view model.autonomousData

        TeleopPage ->
            el
                [ Background.color <| findColor (TeamData.stationToString model.teamData.station)
                , padding 155
                , spacing 10
                , width fill
                , height fill
                , centerY
                ]
            <|
                stylishPage
                    (TeamData.stationToString model.teamData.station)
                    MiddlePage
                    "Teleop"
                    (TeamData.getTeam2 model.teamData
                        |> Result.map String.fromInt
                        |> merge
                    )
                <|
                    Element.map TeleopDataMsg <|
                        Teleop.view model.teleopData

        ClimbingPage ->
            stylishPage
                (TeamData.stationToString model.teamData.station)
                LastPage
                "End-game"
                (TeamData.getTeam2 model.teamData
                    |> Result.map String.fromInt
                    |> merge
                )
            <|
                Element.map ClimbingDataMsg <|
                    Climbing.view model.climbingData


buttonStyle : List (Element.Attribute Msg)
buttonStyle =
    [ Font.color white
    , Font.size 40
    , Font.glow blue 5
    , Border.rounded 10
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
    , width <|
        maximum 350 <|
            fill
    ]
