module UI.Components.Toolbar exposing (Model, toolbarView)

import Element exposing (Element, fill)
import Element.Background
import Element.Border
import UI.Elements
import UI.Styles exposing (ColorHue(..), ColorIntensity(..), Config)
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
        , Element.Background.color (UI.Styles.color Grayscale I100 config)
        ]
        (List.concat
            [ [ Element.el
                    ((UI.Styles.getTypographicStyleFor UI.Styles.H3 config
                        |> UI.Styles.applyFontColor Grayscale I500 config
                     )
                        ++ [ Element.paddingXY 0 8 ]
                    )
                    (Element.text model.title)
              , UI.Elements.spacer UI.Styles.MD
              ]
            , addElementIf model.showCreateAction <| UI.Elements.button UI.Elements.PrimaryLight "Add" create config
            , addElementsIf model.valueChanged <|
                [ UI.Elements.spacer UI.Styles.FILL
                , UI.Elements.button UI.Elements.PrimaryLight "Save" sync config
                , UI.Elements.spacer UI.Styles.XS
                , UI.Elements.button UI.Elements.Subtle "Cancel" cancel config
                ]
            ]
        )
