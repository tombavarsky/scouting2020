module TeamData exposing (Model, Msg, getMatch, getTeam2, init, stationToString, update, view)

import Array exposing (Array)
import Browser
import Colors exposing (blue, orange, sky, white)
import Element exposing (centerX, centerY, column, fill, padding, spacing, text, width)
import Element.Background as Background
import Element.Border exposing (rounded)
import Element.Font as Font exposing (center)
import Element.Input as Input exposing (labelHidden, radio)
import GetMatch exposing (AllianceColor, AllianceStation, Match, StationNumber, getTeam)
import Maybe.Extra exposing (unwrap)
import Result.Extra exposing (merge)
import String


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init <| Array.fromList GetMatch.matches
        , view = Element.layout [] << view
        , update = update
        }


type Msg
    = ScouterInput String
    | MatchInput String
    | Station AllianceStation


type alias Model =
    { scouterName : String
    , matchNumber : String
    , station : Maybe AllianceStation
    , team : Result String Int
    , matches : Array Match
    }


init : Array Match -> Model
init matches =
    Model "" "" Nothing (Err "") matches


inputOption : GetMatch.AllianceColor -> GetMatch.StationNumber -> String -> Input.Option ( AllianceColor, StationNumber ) msg
inputOption allianceColor allianceNumber text =
    Input.option ( allianceColor, allianceNumber ) (Element.text text)


view : Model -> Element.Element Msg
view model =
    column
        [ Background.color sky
        , padding 50
        , spacing 40
        , centerX
        , centerY
        , rounded 20
        ]
        [ textInput model.scouterName ScouterInput "Scouter's name"
        , radio
            [ padding 10
            , spacing 20
            , Font.size 60
            ]
            { onChange = Station
            , selected = model.station
            , label = Input.labelAbove [ Font.size 60, padding 10, spacing 20 ] (text "Which station?")
            , options =
                [ inputOption GetMatch.Blue GetMatch.One "Blue 1"
                , inputOption GetMatch.Blue GetMatch.Two "Blue 2"
                , inputOption GetMatch.Blue GetMatch.Three "Blue 3"
                , inputOption GetMatch.Red GetMatch.One "Red 1"
                , inputOption GetMatch.Red GetMatch.Two "Red 2"
                , inputOption GetMatch.Red GetMatch.Three "Red 3"
                ]
            }
        , textInput model.matchNumber MatchInput "Match number"
        , Element.el
            [ Background.color orange
            , width fill
            , center
            , rounded 10
            , Font.color white
            , Font.glow blue 5
            , Font.size 60
            , Font.family
                [ Font.external
                    { name = "Open Sans"
                    , url = "https://fonts.googleapis.com/css?family=Open+Sans:700i&display=swap"
                    }
                ]
            ]
            (getTeam2 model
                |> Result.map String.fromInt
                |> merge
                |> Element.text
            )
        ]


getTeam2 : Model -> Result String Int
getTeam2 model =
    getMatch model
        |> Result.andThen
            (\match ->
                model.station
                    |> Result.fromMaybe "No station"
                    |> Result.map
                        (\station ->
                            getTeam station match
                        )
            )


getMatch : Model -> Result String Match
getMatch { matchNumber, matches } =
    String.toInt matchNumber
        |> Result.fromMaybe "Match number must be a number"
        |> Result.andThen
            (\number ->
                Array.get (number - 1) matches
                    |> Result.fromMaybe "No such match"
            )


colorToString : AllianceColor -> String
colorToString chosenColor =
    case chosenColor of
        GetMatch.Blue ->
            "Blue"

        GetMatch.Red ->
            "Red"


numberToString : StationNumber -> String
numberToString chosenNumber =
    case chosenNumber of
        GetMatch.One ->
            "1"

        GetMatch.Two ->
            "2"

        GetMatch.Three ->
            "3"


stationToString : Maybe AllianceStation -> String
stationToString alliance =
    unwrap "Now station selected" (\( color, number ) -> String.join " " [ colorToString color, numberToString number ]) alliance


textInput : String -> (String -> Msg) -> String -> Element.Element Msg
textInput modelValue nextButton name =
    Input.text
        [ Font.color sky
        , Font.size 60
        , rounded 10
        , Font.family
            [ Font.external
                { name = "Open Sans"
                , url = "https://fonts.googleapis.com/css?family=Open+Sans:700i&display=swap"
                }
            ]
        ]
        { onChange = nextButton
        , text = modelValue
        , placeholder = Just <| Input.placeholder [] <| Element.text name
        , label = labelHidden modelValue
        }


update : Msg -> Model -> Model
update msg model =
    case msg of
        ScouterInput name ->
            { model | scouterName = name }

        Station chosenStation ->
            { model
                | station = Just chosenStation
                , team = getTeam2 model
            }

        MatchInput matchNumber ->
            { model
                | matchNumber = matchNumber
                , team = getTeam2 model
            }
