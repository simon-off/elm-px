module Dropdown exposing (DropdownModel, view)

import Browser.Events exposing (onKeyDown, onKeyPress)
import Html exposing (..)
import Html.Attributes exposing (class, tabindex)
import Html.Events exposing (onClick, stopPropagationOn)
import Json.Decode exposing (..)
import List exposing (drop)
import Utils exposing (ternary)


type alias Option tValue =
    { label : String
    , value : tValue
    }


type alias DropdownModel tValue =
    { value : tValue
    , isOpen : Bool
    , options : List (Option tValue)
    }


findOptionByValue : DropdownModel tValue -> Maybe (Option tValue)
findOptionByValue dropdown =
    dropdown.options
        |> List.filter (\option -> option.value == dropdown.value)
        |> List.head


onClick2 : msg -> Attribute msg
onClick2 msg =
    stopPropagationOn "click" (Json.Decode.map alwaysPreventDefault (Json.Decode.succeed msg))


alwaysPreventDefault : msg -> ( msg, Bool )
alwaysPreventDefault msg =
    ( msg, True )


viewOption : DropdownModel tValue -> (DropdownModel tValue -> msg) -> Option tValue -> Html msg
viewOption dropdown msg option =
    div
        [ onClick2
            (msg
                { dropdown
                    | value = option.value
                    , isOpen = not dropdown.isOpen
                }
            )
        , class "option"
        , class (ternary (dropdown.value == option.value) "selected" "")
        ]
        [ text option.label ]


view : DropdownModel tValue -> (DropdownModel tValue -> msg) -> Html msg
view dropdown msg =
    div
        [ onClick (msg { dropdown | isOpen = not dropdown.isOpen })
        , class "dropdown"
        , tabindex 0
        ]
        [ div
            [ class "current" ]
            [ text
                (case findOptionByValue dropdown of
                    Just option ->
                        option.label

                    Nothing ->
                        ""
                )
            ]
        , div
            [ class "options"
            , class (ternary dropdown.isOpen "open" "closed")
            ]
            (List.map
                (viewOption dropdown msg)
                dropdown.options
            )
        ]
