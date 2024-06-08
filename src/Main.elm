module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick)



-- UTILS


formatTime : Int -> Int -> String
formatTime hour minute =
    let
        timeString time =
            if time < 10 then
                "0" ++ String.fromInt time

            else
                String.fromInt time
    in
    timeString hour ++ ":" ++ timeString minute



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Task =
    { id : String
    , name : String
    , time : Int
    }


type alias Settings =
    { showWeekend : Bool }


type alias Model =
    { selectedTaskId : String
    , tasks : List Task
    , settings : Settings
    }


weekdays : List String
weekdays =
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
    [ { id = "task-1", name = "task 1", time = 0 }
    , { id = "task-2", name = "task 2", time = 0 }
    , { id = "task-3", name = "task 3", time = 0 }
    ]


init : Model
init =
    { selectedTaskId = ""
    , tasks = tasks
    , settings =
        { showWeekend = True
        }
    }



-- UPDATE


type Action
    = ChangeSelection
    | ChangeSetting


type alias Msg =
    { action : Action
    , data : String
    }


update : Msg -> Model -> Model
update msg model =
    case msg.action of
        ChangeSelection ->
            if model.selectedTaskId == msg.data then
                { model | selectedTaskId = "" }

            else
                { model | selectedTaskId = msg.data }

        ChangeSetting ->
            let
                settings =
                    model.settings

                newSettings condition =
                    { settings | showWeekend = condition }
            in
            if msg.data == "on" then
                { model | settings = newSettings True }

            else
                { model | settings = newSettings False }



-- VIEW


viewTask : Model -> Task -> Html Msg
viewTask model task =
    li
        [ class "task"
        , class
            (if model.selectedTaskId == task.id then
                "selected"

             else
                ""
            )
        , onClick { action = ChangeSelection, data = task.id }
        ]
        [ text task.name ]


viewHour : Int -> List (Html Msg)
viewHour hour =
    List.map
        (\minute ->
            div
                [ class "stroke"
                , class
                    (if minute == 0 then
                        "zero"

                     else
                        ""
                    )
                ]
                [ text (formatTime hour minute) ]
        )
        (List.map (\n -> n * 10) (List.range 0 5))


viewDay : String -> Html Msg
viewDay day =
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
            [ class "strokes" ]
            (List.concat (List.map viewHour (List.range 0 23)))
        ]


view : Model -> Html Msg
view model =
    let
        selectedText =
            case List.head (List.filter (\task -> model.selectedTaskId == task.id) model.tasks) of
                Just asdf ->
                    asdf.name

                _ ->
                    "None"
    in
    div
        []
        [ section
            [ class "section-tasks" ]
            [ h2 [] [ text "Tasks" ]
            , ul
                [ class "tasks" ]
                (List.map (viewTask model) model.tasks)
            , p [] [ text ("Selected: " ++ selectedText) ]
            ]
        , section
            [ class "section-settings" ]
            [ h2 [] [ text "Settings" ]
            , Html.form
                []
                [ label []
                    [ input
                        [ type_ "checkbox"
                        , checked model.settings.showWeekend
                        , onCheck
                            (\checked ->
                                { action = ChangeSetting
                                , data =
                                    if checked then
                                        "on"

                                    else
                                        "off"
                                }
                            )
                        ]
                        []
                    , text "Show weekend"
                    ]
                ]
            ]
        , section
            [ class "section-calendar" ]
            [ h2 [] [ text "Week 25" ]
            , div
                [ class "weekdays" ]
                (List.map viewDay
                    (if model.settings.showWeekend then
                        weekdays

                     else
                        List.take 5 weekdays
                    )
                )
            ]
        ]
