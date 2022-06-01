module UI.Elements exposing (button, spacer, textfield)

import Element exposing (..)
import Element.Background as Background
import Element.Border
import Element.Input as Input
import Json.Decode as Decode
import UI.Styles exposing (Size(..))


spacer : UI.Styles.Size -> Element msg
spacer size =
    case size of
        SM ->
            Element.el [ Element.width (px 16), Element.height (px 16) ] Element.none

        MD ->
            Element.el [ Element.width (px 20), Element.height (px 20) ] Element.none

        LG ->
            Element.el [ Element.width (px 24), Element.height (px 24) ] Element.none

        XL ->
            Element.el [ Element.width (px 40), Element.height (px 40) ] Element.none

        FILL ->
            Element.el [ Element.width fill, Element.height fill ] Element.none


textfield : String -> (String -> msg) -> Element msg
textfield model handler =
    el
        (UI.Styles.getTypographicStyleFor UI.Styles.Body)
        -- (Element.text <|String.fromInt model.viewPort.width)
        (Input.text
            [ spacing 8
            , Element.Border.width 1
            , Element.Border.rounded 4
            , Background.color UI.Styles.color.white
            , Element.Border.color UI.Styles.color.gray300
            , width fill
            , padding 16

            -- , Element.mouseOver <| [ Background.color UI.Styles.color.lightGrey ]
            ]
            { text = model
            , placeholder = Just (Input.placeholder [] (Element.text "Search"))
            , onChange = handler
            , label = Input.labelHidden ""
            }
        )


button : String -> msg -> Element msg
button model msg =
    el
        (UI.Styles.getTypographicStyleFor UI.Styles.Body)
        (Input.button
            [ Background.color UI.Styles.color.gray300
            , padding 14
            , Element.Border.rounded 4
            , Element.mouseOver <| [ Background.color UI.Styles.color.gray300 ]
            ]
            { onPress = Just msg
            , label = text model
            }
        )
