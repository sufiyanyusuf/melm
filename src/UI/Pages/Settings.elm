module UI.Pages.Settings exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Input as Input
import UI.Elements as Elements exposing (button, textfield)
import UI.Pages as Views exposing (Page)
import UI.Styles


type Msg
    = X
    | KeyValueChanged String
    | SaveKeyValue


view : Element Msg
view =
    Element.column
        [ width fill
        , height fill
        , scrollbarY
        , padding 4
        ]
        [ el
            (UI.Styles.getTypographicStyleFor UI.Styles.H1)
            (text (Views.pageTitle Views.Settings))
        , Elements.spacer UI.Styles.XL
        , textfield "hello" KeyValueChanged
        , Elements.spacer UI.Styles.SM
        , button "Save Token" SaveKeyValue
        ]
