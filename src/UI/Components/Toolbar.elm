module UI.Components.Toolbar exposing (Model, toolbarView)

import Element exposing (Element, fill, fillPortion, none, padding, paddingXY, shrink, spacing)
import Element.Background
import Request exposing (RequestStatus)
import UI.Elements
import UI.Styles
import Utils exposing (addElementIf)


type alias Model =
    { valueChanged : Bool
    , requestStatus : RequestStatus
    , showCreateAction : Bool
    , title : String
    }


toolbarView : Model -> msg -> msg -> msg -> Element msg
toolbarView model create sync cancel =
    Element.row
        [ Element.width fill
        , Element.paddingXY 40 12
        , Element.alignTop
        , Element.Background.color UI.Styles.color.gray100
        ]
        (List.concat
            [ [ Element.el (UI.Styles.getTypographicStyleFor UI.Styles.H3) (Element.text model.title)
              , UI.Elements.spacer UI.Styles.MD
              ]
            , addElementIf model.showCreateAction <| UI.Elements.button UI.Elements.Light "Add" create
            ]
            ++ [ UI.Elements.spacer UI.Styles.FILL
               , UI.Elements.button UI.Elements.Clear "Save" sync
               , UI.Elements.button UI.Elements.Clear "Cancel" cancel
               ]
        )
