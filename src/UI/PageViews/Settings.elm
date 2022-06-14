module UI.PageViews.Settings exposing (..)

import Element exposing (..)
import UI.Elements as Elements exposing (button, textfield)
import UI.Styles


type Msg
    = KeyValueChanged String
    | SaveKeyValue
    | None


type alias Model =
    { tokenValue : String, title : String }


init : Model
init =
    { tokenValue = "", title = "Settings" }


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
        , textfield model.tokenValue "Token" KeyValueChanged None None
        , Elements.spacer UI.Styles.SM
        , button "Save Token" SaveKeyValue
        ]
