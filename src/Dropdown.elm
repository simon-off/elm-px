module Dropdown exposing (view)

import Browser.Events exposing (onKeyDown, onKeyPress)
import Html exposing (..)
import Html.Attributes exposing (class, tabindex)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Utils exposing (ternary)


type alias Option =
    { label : String
    , value : String
    }


keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.field "key" Decode.string


findOptionByValue : List Option -> String -> Option
findOptionByValue options value =
    Maybe.withDefault
        { value = "", label = "" }
        (options
            |> List.filter (\option -> option.value == value)
            |> List.head
        )


viewOption : (String -> msg) -> String -> Option -> Html msg
viewOption msg value option =
    div
        [ onClick (msg option.value)
        , class "option"
        , class (ternary (value == option.value) "selected" "")
        ]
        [ text option.label ]


view : Bool -> List Option -> String -> (String -> msg) -> msg -> Html msg
view open options value selectMsg toggleMsg =
    div [ onClick toggleMsg, class "dropdown", tabindex 0 ]
        [ div
            [ class "value" ]
            [ text (findOptionByValue options value).label ]
        , div
            [ class "options"
            , class (ternary open "open" "closed")
            ]
            (List.map
                (viewOption selectMsg value)
                options
            )
        ]
