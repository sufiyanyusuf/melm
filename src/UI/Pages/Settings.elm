module UI.Pages.Settings exposing (..)

import Element exposing (..)
import UI.Elements as Elements exposing (button, textfield)
import UI.Styles


type Msg
    = X
    | KeyValueChanged String
    | SaveKeyValue


type alias Model =
    { token : String, title : String }


init : Model
init =
    { token = "", title = "Settings" }


view : Model -> Element Msg
view model =
    Element.column
        [ width fill
        , height fill
        , scrollbarY
        , padding 4
        ]
        [ el
            (UI.Styles.getTypographicStyleFor UI.Styles.H1)
            (text model.title)
        , Elements.spacer UI.Styles.XL
        , textfield model.token KeyValueChanged
        , Elements.spacer UI.Styles.SM
        , button "Save Token" SaveKeyValue
        ]
