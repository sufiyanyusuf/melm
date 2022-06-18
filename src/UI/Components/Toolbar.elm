module UI.Components.Toolbar exposing (Model, toolbarView)

import Element exposing (Element, fill)
import Element.Background
import UI.Elements
import UI.Styles exposing (Config)
import Utils exposing (addElementIf, addElementsIf)


type alias Model =
    { valueChanged : Bool
    , loading : Bool
    , showCreateAction : Bool
    , title : String
    }


toolbarView : Model -> msg -> msg -> msg -> Config -> Element msg
toolbarView model create sync cancel config =
    Element.row
        [ Element.width fill
        , Element.paddingXY 40 12
        , Element.alignTop
        , Element.Background.color (UI.Styles.color config).gray100
        ]
        (List.concat
            [ [ Element.el
                    (UI.Styles.getTypographicStyleFor UI.Styles.H3 config ++ [ Element.paddingXY 0 8 ])
                    (Element.text model.title)
              , UI.Elements.spacer UI.Styles.MD
              ]
            , addElementIf model.showCreateAction <| UI.Elements.button UI.Elements.PrimaryLight "Add" create config
            , addElementsIf model.valueChanged <|
                [ UI.Elements.spacer UI.Styles.FILL
                , UI.Elements.button UI.Elements.Clear "Save" sync config
                , UI.Elements.button UI.Elements.Clear "Cancel" cancel config
                ]
            ]
        )