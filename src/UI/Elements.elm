module UI.Elements exposing (Theme(..), button, chip, iconButton, spacer, switch, syncIndicator, textfield)

import Element exposing (..)
import Element.Background as Background
import Element.Border exposing (rounded)
import Element.Events exposing (onClick, onLoseFocus)
import Element.Input as Input exposing (OptionState(..))
import Expect exposing (true)
import Html.Events
import Json.Decode as Decode
import Request exposing (RequestStatus(..))
import UI.Icons exposing (Icon(..), Style(..), buildIcon)
import UI.Styles exposing (Size(..))


spacer : UI.Styles.Size -> Element msg
spacer size =
    case size of
        XS ->
            Element.el [ Element.width (px 8), Element.height (px 8) ] Element.none

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


textfield : String -> String -> (String -> msg) -> msg -> msg -> Element msg
textfield value placeholder valueChanged loseFocus returnKeyMsg =
    el
        (UI.Styles.getTypographicStyleFor UI.Styles.Body
            ++ [ Element.width fill, onEnter returnKeyMsg ]
        )
        (Input.text
            [ spacing 8
            , Element.Border.width 0
            , Element.Border.rounded 6
            , Background.color UI.Styles.color.white
            , Element.Border.color UI.Styles.color.gray300
            , width fill
            , padding 12
            , onLoseFocus loseFocus
            ]
            { text = value
            , placeholder = Just (Input.placeholder [] (Element.text placeholder))
            , onChange = valueChanged
            , label = Input.labelHidden ""
            }
        )


onEnter : msg -> Element.Attribute msg
onEnter msg =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Decode.succeed msg

                        else
                            Decode.fail "Not the enter key"
                    )
            )
        )


switch : Bool -> msg -> Element msg
switch model msg =
    Element.el
        [ Element.inFront (switchHandle model)
        , pointer
        , onClick msg
        ]
        (switchBody model)


switchHandle : Bool -> Element msg
switchHandle model =
    let
        position =
            if model == True then
                20

            else
                4
    in
    el
        [ Background.color UI.Styles.color.white
        , width (px 24)
        , Element.height (px 24)
        , Element.Border.rounded 12
        , Element.centerY
        , Element.moveRight position
        ]
        Element.none


switchBody : Bool -> Element msg
switchBody model =
    let
        background =
            if model == True then
                UI.Styles.color.green500

            else
                UI.Styles.color.gray300
    in
    el
        [ Background.color background
        , width (px 48)
        , Element.height (px 32)
        , Element.Border.rounded 20
        ]
        Element.none


type Theme
    = Subtle
    | Clear


button : Theme -> String -> msg -> Element msg
button buttonTheme model msg =
    let
        props =
            getButtonProps buttonTheme
    in
    el
        (UI.Styles.getTypographicStyleFor UI.Styles.Body)
        (Input.button
            [ Background.color props.bgColor
            , paddingEach { top = 10, right = 12, bottom = 8, left = 12 }
            , Element.Border.rounded 6
            , Element.mouseOver <| [ Background.color props.hoverColor ]
            ]
            { onPress = Just msg
            , label = text model
            }
        )


getButtonProps : Theme -> ButtonProps
getButtonProps buttonType =
    case buttonType of
        Subtle ->
            { bgColor = UI.Styles.color.white
            , hoverColor = UI.Styles.color.gray300
            }

        Clear ->
            { bgColor = UI.Styles.color.clear
            , hoverColor = UI.Styles.color.gray300
            }


type alias ButtonProps =
    { bgColor : Color
    , hoverColor : Color
    }


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


chip : String -> RequestStatus -> Bool -> msg -> Element msg
chip text requestStatus saved msg =
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
            [ syncIndicator requestStatus (not saved)
            , Element.text text
            , el
                [ padding 8
                , Element.Border.rounded 5
                , pointer
                , Element.Events.onClick msg
                , Element.mouseOver <| [ alpha 0.3 ]
                ]
                (buildIcon UI.Icons.Close Outline)
            ]
        )


syncIndicator : RequestStatus -> Bool -> Element msg
syncIndicator status valueChanged =
    if valueChanged then
        case status of
            NoRequest ->
                el
                    [ Background.color UI.Styles.color.primary200
                    , width (px 8)
                    , Element.height (px 8)
                    , Element.Border.rounded 12
                    , Element.centerY
                    ]
                    (text "")

            Fired ->
                el
                    [ Background.color UI.Styles.color.primary500
                    , width (px 8)
                    , Element.height (px 8)
                    , Element.Border.rounded 12
                    , Element.centerY
                    ]
                    (text "")

            Success ->
                el
                    [ Background.color UI.Styles.color.primary200
                    , width (px 8)
                    , Element.height (px 8)
                    , Element.Border.rounded 12
                    , Element.centerY
                    ]
                    (text "")

            Failed ->
                el
                    [ Background.color UI.Styles.color.green500
                    , width (px 8)
                    , Element.height (px 8)
                    , Element.Border.rounded 12
                    , Element.centerY
                    ]
                    (text "")

    else
        Element.none
