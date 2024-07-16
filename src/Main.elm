module Main exposing (Msg(..), main)

import Array exposing (Array)
import Browser
import Dict exposing (Dict)
import Dropdown
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onCheck, onClick, onMouseDown, onMouseOver, onMouseUp)
import Json.Decode as Decode exposing (Decoder, field, int)
import Maybe exposing (withDefault)
import Set exposing (Set)
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
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- MODEL


type alias Settings =
    { showWeekend : Bool
    , timeIncrement : Int
    }


type alias TaskId =
    Int


type alias Task =
    { id : TaskId
    , color : String
    , name : String
    }


type alias Day =
    Array (Maybe TaskId)


type alias Week =
    Array Day


type alias Model =
    { isDrawing : Bool
    , selectedTask : Maybe Task
    , tasks : List Task
    , settings : Settings
    , week : Week
    , timeIncrementDropdown : Dropdown.DropdownModel Int
    }


emptyTask : Task
emptyTask =
    { id = 0
    , color = "transparent"
    , name = ""
    }


defaultTasks : List Task
defaultTasks =
    List.map
        (\n ->
            { id = n
            , color = "oklch(70% 40% " ++ String.fromInt (n * 50 + 180) ++ ")"
            , name = "Task " ++ String.fromInt n
            }
        )
        [ 1, 2, 3, 4, 5, 6, 7 ]


emptyDay : Day
emptyDay =
    -- Divide the day into 6 minute segments
    Array.initialize 240 (\_ -> Nothing)


defaultTimeIncrement : Int
defaultTimeIncrement =
    6


init : Model
init =
    { isDrawing = False
    , selectedTask = Nothing
    , tasks = defaultTasks
    , settings =
        { showWeekend = True
        , timeIncrement = defaultTimeIncrement
        }
    , week = Array.initialize 7 (\_ -> emptyDay)
    , timeIncrementDropdown =
        { isOpen = False
        , value = 6
        , options =
            [ { value = 6, label = "6" }
            , { value = 10, label = "10" }
            , { value = 15, label = "15" }
            , { value = 30, label = "30" }
            , { value = 60, label = "60" }
            ]
        }
    }


type alias TimeSlotMessage =
    { model : Model
    , slotIndex : Int
    , slot : Maybe Int
    , dayIndex : Int
    , day : Day
    , startDrawing : Bool
    }



-- UPDATE


type Msg
    = UpdateSelectedTask Task
    | UpdateShowWeekend Bool
    | UpdateDropdownTimeIncrement (Dropdown.DropdownModel Int)
    | FillTimeSlot TimeSlotMessage
    | StopDrawing


updateSettings : (Settings -> Settings) -> Model -> Model
updateSettings transform model =
    { model | settings = transform model.settings }


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateSelectedTask data ->
            ternary (model.selectedTask == Just data)
                { model | selectedTask = Nothing }
                { model | selectedTask = Just data }

        UpdateShowWeekend data ->
            ternary data
                (model |> updateSettings (\x -> { x | showWeekend = True }))
                (model |> updateSettings (\x -> { x | showWeekend = False }))

        StopDrawing ->
            { model | isDrawing = False }

        FillTimeSlot conf ->
            if model.isDrawing || conf.startDrawing then
                let
                    newDay =
                        case model.selectedTask of
                            Nothing ->
                                conf.day |> Array.set conf.slotIndex Nothing

                            Just task ->
                                conf.day |> Array.set conf.slotIndex (Just task.id)

                    newWeek =
                        Array.set conf.dayIndex newDay model.week
                in
                { model
                    | week = newWeek
                    , isDrawing =
                        if conf.startDrawing then
                            True

                        else
                            model.isDrawing
                }

            else
                model

        UpdateDropdownTimeIncrement dropdown ->
            { model | timeIncrementDropdown = dropdown }



-- VIEW


viewTask : Model -> Int -> Task -> Html Msg
viewTask model index task =
    li
        [ class "task"
        , class (ternary (model.selectedTask == Just task) "selected" "")
        , onClick (UpdateSelectedTask task)
        ]
        [ p
            [ class "index"
            , attribute "style" ("--clr: " ++ task.color)
            ]
            [ text (String.fromInt (index + 1)) ]
        , p [] [ text task.name ]
        , p [] [ text task.name ]
        ]


viewSlot :
    { model : Model
    , slotIndex : Int
    , slot : Maybe Int
    , dayIndex : Int
    , day : Day
    }
    -> Html Msg
viewSlot conf =
    let
        message =
            { model = conf.model
            , slotIndex = conf.slotIndex
            , slot = conf.slot
            , dayIndex = conf.dayIndex
            , day = conf.day
            , startDrawing = False
            }

        minute =
            conf.slotIndex * 6

        task =
            conf.model.tasks
                |> List.filter (\t -> t.id == Maybe.withDefault -1 conf.slot)
                |> List.head
                |> Maybe.withDefault emptyTask
    in
    div
        [ class "stroke"
        , class (ternary (String.endsWith "00" (String.fromInt minute)) "zero" "")
        , onMouseDown (FillTimeSlot { message | startDrawing = True })
        , onMouseOver (FillTimeSlot message)
        ]
        [ div [ class "time" ] [ text (getTimeString minute) ]
        , div [ class "task", attribute "style" ("--clr: " ++ task.color) ]
            [ text task.name ]
        ]


viewDay : Model -> Int -> Day -> Html Msg
viewDay model dayIndex day =
    let
        dayNames =
            Array.fromList
                [ "monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday" ]
    in
    div
        [ class "day" ]
        [ header
            []
            [ h3
                [ class "name" ]
                [ text (Maybe.withDefault "" (Array.get dayIndex dayNames)) ]
            , p
                [ class "date" ]
                [ text "25/4" ]
            ]
        , div
            [ class "strokes" ]
            (day
                |> Array.indexedMap (\i x -> ( i, x ))
                |> Array.slice 120 150
                |> Array.map
                    (\( index, slot ) ->
                        viewSlot
                            { model = model
                            , slotIndex = index
                            , slot = slot
                            , dayIndex = dayIndex
                            , day = day
                            }
                    )
                |> Array.toList
            )
        ]


viewSettings : Model -> Html Msg
viewSettings model =
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
                        (\checked -> UpdateShowWeekend checked)
                    ]
                    []
                , text "Show weekend"
                ]
            , label [ class "dropdown-time" ]
                [ text "Time increment"
                , Dropdown.view model.timeIncrementDropdown UpdateDropdownTimeIncrement
                ]
            ]
        ]


view : Model -> Html Msg
view model =
    main_
        [ onMouseUp StopDrawing ]
        [ section
            [ class "section-tasks" ]
            [ h2 [] [ text (ternary model.isDrawing "JA" "NEJ") ]
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
                (model.week
                    |> doIf (Array.slice 0 5) (not model.settings.showWeekend)
                    |> Array.indexedMap (viewDay model)
                    |> Array.toList
                )
            ]
        ]
