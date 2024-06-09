module Main exposing (main)

import Browser
import Dropdown
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onInput)
import Utils exposing (doIf, ternary)



-- UTILS


getTimeString : Int -> String
getTimeString minute =
    let
        hourString =
            String.fromInt (minute // 60)

        minuteString =
            String.fromInt (remainderBy 60 minute)

        padTimeString str =
            ternary (String.length str == 1) ("0" ++ str) str
    in
    padTimeString hourString ++ ":" ++ padTimeString minuteString


isHour : String -> Bool
isHour timeStr =
    String.endsWith "00" timeStr


getStrokes : Int -> List String
getStrokes timeIncrement =
    List.map
        getTimeString
        (List.filter
            (\n -> remainderBy timeIncrement n == 0)
            (List.range 0 (1440 - 1))
        )



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Task =
    { id : Int
    , name : String
    , time : Int
    }


type alias Settings =
    { showWeekend : Bool
    , timeIncrement : Int
    }


type alias Entry =
    { taskId : String
    , start : Int
    , end : Int
    }


type alias Model =
    { selectedTaskId : Maybe Int
    , tasks : List Task
    , settings : Settings
    , week :
        { monday : List Entry
        , tuesday : List Entry
        , wednesday : List Entry
        , thursday : List Entry
        , friday : List Entry
        , saturday : List Entry
        , sunday : List Entry
        }
    , strokes : List String
    , timeIncrementOpen : Bool
    }


days : List String
days =
    [ "monday"
    , "tuesday"
    , "wednesday"
    , "thursday"
    , "friday"
    , "saturday"
    , "sunday"
    ]


tasks : List Task
tasks =
    [ { id = 1, name = "task 1", time = 0 }
    , { id = 2, name = "task 2", time = 0 }
    , { id = 3, name = "task 3", time = 0 }
    , { id = 4, name = "task 3", time = 0 }
    , { id = 5, name = "task 3", time = 0 }
    , { id = 6, name = "task 3", time = 0 }
    , { id = 7, name = "task 3", time = 0 }
    , { id = 8, name = "task 3", time = 0 }
    , { id = 9, name = "task 3", time = 0 }
    , { id = 10, name = "task 3", time = 0 }
    , { id = 11, name = "task 3", time = 0 }
    , { id = 12, name = "task 3", time = 0 }
    , { id = 13, name = "task 3", time = 0 }
    , { id = 14, name = "task 3", time = 0 }
    , { id = 15, name = "task 3", time = 0 }
    ]


defaultTimeIncrement : Int
defaultTimeIncrement =
    30


init : Model
init =
    { selectedTaskId = Nothing
    , tasks = tasks
    , settings =
        { showWeekend = True
        , timeIncrement = defaultTimeIncrement
        }
    , week =
        { monday = []
        , tuesday = []
        , wednesday = []
        , thursday = []
        , friday = []
        , saturday = []
        , sunday = []
        }
    , strokes = getStrokes defaultTimeIncrement
    , timeIncrementOpen = False
    }



-- UPDATE


type Action
    = ChangeSelection Int
    | ChangeShowWeekend Bool
    | ChangeTimeIncrement String
    | ToggleTimeIncrementDropdown


type alias Msg =
    Action


updateSettings : (Settings -> Settings) -> Model -> Model
updateSettings transform model =
    { model | settings = transform model.settings }


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeSelection data ->
            ternary (model.selectedTaskId == Just data)
                { model | selectedTaskId = Nothing }
                { model | selectedTaskId = Just data }

        ChangeShowWeekend data ->
            ternary data
                (model |> updateSettings (\x -> { x | showWeekend = True }))
                (model |> updateSettings (\x -> { x | showWeekend = False }))

        ChangeTimeIncrement data ->
            let
                newModel =
                    model |> updateSettings (\x -> { x | timeIncrement = Maybe.withDefault 10 (String.toInt data) })
            in
            { newModel | strokes = getStrokes newModel.settings.timeIncrement }

        ToggleTimeIncrementDropdown ->
            { model | timeIncrementOpen = not model.timeIncrementOpen }



-- VIEW


viewTask : Model -> Int -> Task -> Html Msg
viewTask model index task =
    li
        [ class "task"
        , class (ternary (model.selectedTaskId == Just task.id) "selected" "")
        , onClick (ChangeSelection task.id)
        ]
        [ p
            [ class "index"
            , attribute "style" ("--clr: oklch(70% 40% " ++ String.fromInt (index * 50) ++ ")")
            ]
            [ text (String.fromInt (index + 1)) ]
        , p [] [ text task.name ]
        , p [] [ text task.name ]
        ]


viewStroke : String -> Html Msg
viewStroke stroke =
    div
        [ class "stroke"
        , class (ternary (isHour stroke) "zero" "")
        ]
        [ text stroke ]


viewDay : Model -> String -> Html Msg
viewDay model day =
    div
        [ class "weekday" ]
        [ header
            []
            [ h3
                [ class "name" ]
                [ text day ]
            , p
                [ class "date" ]
                [ text "25/4" ]
            ]
        , div
            [ class "strokes", attribute "day" day ]
            (List.map
                viewStroke
                model.strokes
            )
        ]


viewSettings : Model -> Html Msg
viewSettings model =
    let
        isSelected value =
            case String.toInt value of
                Just number ->
                    number == model.settings.timeIncrement

                Nothing ->
                    False

        timeOptions =
            [ "10", "15", "20", "30", "60" ]
    in
    section
        [ class "section-settings" ]
        [ h2 [] [ text "Settings" ]
        , Html.form
            []
            [ label [ class "checkbox-weekend" ]
                [ input
                    [ type_ "checkbox"
                    , checked model.settings.showWeekend
                    , onCheck
                        (\checked -> ChangeShowWeekend checked)
                    ]
                    []
                , text "Show weekend"
                ]
            , label [ class "dropdown-time" ]
                [ text "Time increment"
                , Dropdown.view
                    model.timeIncrementOpen
                    (timeOptions |> List.map (\x -> { value = x, label = x }))
                    (String.fromInt model.settings.timeIncrement)
                    ChangeTimeIncrement
                    ToggleTimeIncrementDropdown
                ]
            ]
        ]


view : Model -> Html Msg
view model =
    let
        selectedText =
            case List.head (model.tasks |> List.filter (\task -> model.selectedTaskId == Just task.id)) of
                Just task ->
                    task.name

                Nothing ->
                    "None"
    in
    div
        []
        [ section
            [ class "section-tasks" ]
            [ h2 [] [ text "Tasks" ]
            , ul
                [ class "tasks" ]
                ([ [ li [ class "task header" ]
                        [ p []
                            [ text "#" ]
                        , p [] [ text "Name" ]
                        , p [] [ text "Other" ]
                        ]
                   ]
                 , model.tasks |> List.indexedMap (viewTask model)
                 ]
                    |> List.concat
                )
            ]
        , viewSettings model
        , section
            [ class "section-calendar" ]
            [ h2 [] [ text "Calendar" ]
            , div
                [ class "week" ]
                (List.map (viewDay model) (days |> doIf (List.take 5) (not model.settings.showWeekend)))
            ]
        ]
