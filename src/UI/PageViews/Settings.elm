module UI.PageViews.Settings exposing (..)

import Element exposing (..)
import Request exposing (RequestStatus(..))
import UI.Components.Toolbar
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
        [ height fill
        , width fill
        , paddingEach { top = 20, bottom = 12, left = 0, right = 0 }
        , inFront (toolbarView model)
        ]
        [ Element.column
            [ width fill
            , height fill
            , scrollbarY
            , paddingXY 120 40
            ]
            [ Elements.spacer UI.Styles.XL
            , textfield model.tokenValue "Token" KeyValueChanged None None
            , Elements.spacer UI.Styles.SM
            , button Elements.Light "Save Token" SaveKeyValue
            ]
        ]


toolbarView : Model -> Element Msg
toolbarView _ =
    let
        toolbarModel =
            { valueChanged = False
            , requestStatus = NoRequest
            , showCreateAction = False
            , title = "Settings"
            }
    in
    UI.Components.Toolbar.toolbarView toolbarModel None SaveKeyValue None
