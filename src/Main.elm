module Main exposing (main)

import Array exposing (Array)
import Browser
import Dict exposing (Dict)
import Dropdown
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onMouseDown, onMouseOver, onMouseUp)
import Maybe exposing (withDefault)
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


type alias Task =
    { id : Int
    , name : String
    , time : Int
    }


type alias Settings =
    { showWeekend : Bool
    , timeIncrement : Int
    }


type alias Day =
    Array (Maybe Int)


type alias Model =
    { selectedTaskId : Maybe Int
    , tasks : List Task
    , settings : Settings
    , week : Array Day
    , strokes : List String
    , timeIncrementOpen : Bool
    , isMouseDown : Bool
    }


tasks : List Task
tasks =
    [ { id = 1, name = "task 1", time = 0 }
    , { id = 2, name = "task 2", time = 0 }
    , { id = 3, name = "task 3", time = 0 }
    ]


defaultTimeIncrement : Int
defaultTimeIncrement =
    30


emptyDay : Day
emptyDay =
    Array.initialize 240 (\_ -> Nothing)


init : Model
init =
    { selectedTaskId = Nothing
    , tasks = tasks
    , settings =
        { showWeekend = True
        , timeIncrement = defaultTimeIncrement
        }
    , week = Array.initialize 7 (\_ -> emptyDay)
    , strokes = getStrokes defaultTimeIncrement
    , timeIncrementOpen = False
    , isMouseDown = False
    }



-- UPDATE


type Msg
    = ChangeSelection Int
    | ChangeShowWeekend Bool
    | ChangeTimeIncrement String
    | ToggleTimeIncrementDropdown
    | SetMouseDown Bool
    | Default
    | FillTimeSlot
        { slotIndex : Int
        , slot : Maybe Int
        , dayIndex : Int
        , day : Day
        }


updateSettings : (Settings -> Settings) -> Model -> Model
updateSettings transform model =
    { model | settings = transform model.settings }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Default ->
            model

        SetMouseDown down ->
            { model | isMouseDown = down }

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
                    model
                        |> updateSettings
                            (\x -> { x | timeIncrement = Maybe.withDefault 10 (String.toInt data) })
            in
            { newModel | strokes = getStrokes newModel.settings.timeIncrement }

        ToggleTimeIncrementDropdown ->
            { model | timeIncrementOpen = not model.timeIncrementOpen }

        FillTimeSlot conf ->
            let
                newDay =
                    Array.set conf.slotIndex model.selectedTaskId conf.day

                newWeek =
                    Array.set conf.dayIndex newDay model.week
            in
            ternary model.isMouseDown { model | week = newWeek } model



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
            , attribute "style" ("--clr: oklch(70% 40% " ++ String.fromInt (index * 50 + 180) ++ ")")
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
        minute =
            conf.slotIndex * 6

        taskName =
            conf.model.tasks
                |> List.filter (\task -> task.id == Maybe.withDefault -1 conf.slot)
                |> List.head
                |> (\o ->
                        case o of
                            Just task ->
                                task.name

                            Nothing ->
                                ""
                   )
    in
    div
        [ class "stroke"
        , class (ternary (isHour (getTimeString minute)) "zero" "")
        , onMouseOver
            (FillTimeSlot
                { slotIndex = conf.slotIndex
                , slot = conf.slot
                , dayIndex = conf.dayIndex
                , day = conf.day
                }
            )
        ]
        [ div [ class "time" ] [ text (getTimeString minute) ]
        , div [ class "task" ]
            [ text taskName ]
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
                |> Array.indexedMap
                    (\index slot ->
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
    let
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
    main_
        [ onMouseDown (SetMouseDown True)
        , onMouseUp (SetMouseDown False)
        ]
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
                (model.week
                    |> doIf (Array.slice 0 5) (not model.settings.showWeekend)
                    |> Array.indexedMap (viewDay model)
                    |> Array.toList
                )
            ]
        ]
