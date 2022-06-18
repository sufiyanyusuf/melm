module Utils exposing (..)

import Element exposing (Attribute, Element)


addIf : Bool -> Attribute msg -> List (Attribute msg)
addIf isNeed attr =
    if isNeed then
        [ attr ]

    else
        []


addElementIf : Bool -> Element msg -> List (Element msg)
addElementIf isNeed element =
    if isNeed then
        [ element ]

    else
        []


addElementsIf : Bool -> List (Element msg) -> List (Element msg)
addElementsIf isNeed elements =
    if isNeed then
        elements

    else
        []
