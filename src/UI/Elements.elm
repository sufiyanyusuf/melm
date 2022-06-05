module UI.Elements exposing (button, chip, iconButton, spacer, textfield)

import Chart.Attributes exposing (background, opacity)
import Element exposing (Element, el, fill, padding, paddingEach, pointer, px, spacing, text, width)
import Element.Background as Background
import Element.Border
import Element.Events exposing (onClick, onLoseFocus)
import Element.Input as Input
import Svg.Attributes exposing (fillOpacity)
import UI.Icons exposing (Icon, Style(..), buildIcon)
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


textfield : String -> String -> (String -> msg) -> msg -> Element msg
textfield value placeholder valueChanged loseFocus =
    el
        (UI.Styles.getTypographicStyleFor UI.Styles.Body)
        -- (Element.text <|String.fromInt model.viewPort.width)
        (Input.text
            [ spacing 8
            , Element.Border.width 0
            , Element.Border.rounded 6
            , Background.color UI.Styles.color.white
            , Element.Border.color UI.Styles.color.gray300
            , width fill
            , padding 12
            , onLoseFocus loseFocus

            -- , Element.mouseOver <| [ Background.color UI.Styles.color.lightGrey ]
            ]
            { text = value
            , placeholder = Just (Input.placeholder [] (Element.text placeholder))
            , onChange = valueChanged
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


iconButton : Icon -> msg -> Element msg
iconButton icon msg =
    el
        [ padding 8
        , Element.Border.rounded 6
        , pointer
        , Element.Events.onClick msg
        , Element.mouseOver [ Background.color UI.Styles.color.gray100 ]
        , Element.mouseDown [ Background.color UI.Styles.color.gray300 ]
        ]
        (buildIcon icon Outline)


chip : String -> msg -> Element msg
chip text msg =
    el
        (UI.Styles.getTypographicStyleFor UI.Styles.Body)
        (Element.row
            [ Background.color UI.Styles.color.white
            , paddingEach { top = 4, left = 12, bottom = 4, right = 4 }
            , Element.Border.rounded 6
            , Element.mouseOver <| [ Background.color UI.Styles.color.gray100 ]
            , Element.Border.color UI.Styles.color.gray300
            , Element.Border.width 1
            , spacing 4
            ]
            [ Element.text text
            , el
                [ --     Background.color UI.Styles.color.white
                  -- , mouseOver [ Background.color UI.Styles.color.gray100 ]
                  padding 8
                , Element.Border.rounded 5
                , pointer
                , Element.Events.onClick msg
                ]
                (buildIcon UI.Icons.Close Outline)
            ]
        )
