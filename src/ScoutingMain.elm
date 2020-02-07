module ScoutingMain exposing (Model, Msg, init, update, view)

import Array
import Autonomous
import Browser
import Browser.Events as BE
import Climbing
import Colors exposing (blue, purple, white)
import Element exposing (Device, centerX, centerY, column, el, fill, height, layout, padding, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font exposing (center)
import Element.Input exposing (button)
import GetMatch
import Html.Attributes exposing (style)
import TeamData
import Teleop


main : Program () Model Msg
main =
    Browser.element
        { init = always ( init, Cmd.none )
        , view = view >> layout [ width fill, htmlAttribute <| style "touch-action" "manipulation" ]
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
    | ScreenSize Device
    | PrevPage
    | NextPage


type PagePosition
    = FirstPage
    | MiddlePage
    | LastPage


type alias DeviceSize =
    { width : Int
    , height : Int
    }


type alias Model =
    { teamData : TeamData.Model
    , autonomousData : Autonomous.Model
    , teleopData : Teleop.Model
    , climbingData : Climbing.Model
    , pages : Pages
    , screenSize : Device
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
        , spacing 15
        , width fill
        , height fill
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
                column [ spacing 15, centerX, centerY ]
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
    , screenSize = Element.classifyDevice <| DeviceSize 0 0
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        ScreenSize device ->
            { model | screenSize = device }

        TeamDataMsg teamMsg ->
            { model | teamData = TeamData.update teamMsg model.teamData }

        AutonomousDataMsg autoMsg ->
            { model | autonomousData = Autonomous.update autoMsg model.autonomousData }

        TeleopDataMsg telMsg ->
            { model | teleopData = Teleop.update telMsg model.teleopData }

        ClimbingDataMsg climbMsg ->
            { model | climbingData = Climbing.update climbMsg model.climbingData }

        PrevPage ->
            case model.pages of
                AutonomousPage ->
                    { model | pages = TeamDataPage }

                TeleopPage ->
                    { model | pages = AutonomousPage }

                ClimbingPage ->
                    { model | pages = TeleopPage }

                TeamDataPage ->
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
    case model.pages of
        TeamDataPage ->
            stylishPage (TeamData.stationToString model.teamData.station) FirstPage "Registeration" (TeamData.stationToString model.teamData.station) <| Element.map TeamDataMsg <| TeamData.view model.teamData

        AutonomousPage ->
            el
                [ Background.color <| findColor (TeamData.stationToString model.teamData.station)
                , padding 105
                , spacing 10
                , width fill
                , height fill
                , centerY
                , centerX
                ]
            <|
                stylishPage (TeamData.stationToString model.teamData.station) MiddlePage "Autonomous" (TeamData.stationToString model.teamData.station) <|
                    Element.map AutonomousDataMsg <|
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
                stylishPage (TeamData.stationToString model.teamData.station) MiddlePage "Teleop" (TeamData.stationToString model.teamData.station) <|
                    Element.map TeleopDataMsg <|
                        Teleop.view model.teleopData

        ClimbingPage ->
            stylishPage (TeamData.stationToString model.teamData.station) LastPage "End-game" (TeamData.stationToString model.teamData.station) <| Element.map ClimbingDataMsg <| Climbing.view model.climbingData


buttonStyle : List (Element.Attribute Msg)
buttonStyle =
    [ Font.color white
    , Font.size 60
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
    ]
