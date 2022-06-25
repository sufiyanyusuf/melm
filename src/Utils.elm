module Utils exposing (..)

import Array exposing (Array)
import Element exposing (Attribute, Element)


remove : Int -> Array a -> Array a
remove i a =
    let
        a1 =
            Array.slice 0 i a

        a2 =
            Array.slice (i + 1) (Array.length a) a
    in
    Array.append a1 a2


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
