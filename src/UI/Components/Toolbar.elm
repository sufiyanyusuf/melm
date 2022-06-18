module UI.Components.Toolbar exposing (Model, toolbarView)

import Element exposing (Element, fill)
import Element.Background
import UI.Elements
import UI.Styles
import Utils exposing (addElementIf, addElementsIf)


type alias Model =
    { valueChanged : Bool
    , loading : Bool
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
            [ [ Element.el (UI.Styles.getTypographicStyleFor UI.Styles.H3 ++ [ Element.paddingXY 0 20 ]) (Element.text model.title)
              , UI.Elements.spacer UI.Styles.MD
              ]
            , addElementIf model.showCreateAction <| UI.Elements.button UI.Elements.Subtle "Add" create
            , addElementsIf model.valueChanged <|
                [ UI.Elements.spacer UI.Styles.FILL
                , UI.Elements.button UI.Elements.Clear "Save" sync
                , UI.Elements.button UI.Elements.Clear "Cancel" cancel
                ]
            ]
         -- , addElementsIf model.valueChanged <|
         --     [ UI.Elements.spacer UI.Styles.FILL
         --     , UI.Elements.button UI.Elements.Clear "Save" sync
         --     , UI.Elements.button UI.Elements.Clear "Cancel" cancel
         --     ]
        )
