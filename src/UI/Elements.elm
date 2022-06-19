module UI.Elements exposing (Theme(..), button, chip, iconButton, spacer, switch, syncIndicator, textfield)

import Element exposing (..)
import Element.Background as Background
import Element.Border exposing (rounded)
import Element.Events exposing (onClick, onLoseFocus)
import Element.Font
import Element.Input as Input exposing (OptionState(..))
import Expect exposing (true)
import Html.Events exposing (onMouseOver)
import Json.Decode as Decode
import Request exposing (RequestStatus(..))
import UI.Icons exposing (Icon(..), Style(..), buildIcon)
import UI.Styles exposing (Config, Size(..))


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


textfield : String -> String -> String -> (String -> msg) -> msg -> msg -> Config -> Element msg
textfield value label placeholder valueChanged loseFocus returnKeyMsg config =
    el
        (UI.Styles.getTypographicStyleFor UI.Styles.Body config
            ++ [ Element.width fill
               , onEnter returnKeyMsg
               ]
        )
        (Input.text
            [ spacing 8
            , Element.Border.width 0
            , Element.Border.rounded 6
            , Background.color (UI.Styles.color config).white
            , Element.Border.color (UI.Styles.color config).gray200
            , Element.Border.width 1
            , width fill
            , paddingXY 12 14
            , onLoseFocus loseFocus
            , Element.mouseOver <| [ Background.color (UI.Styles.color config).gray200 ]
            ]
            { text = value
            , placeholder = Just (Input.placeholder [] (Element.text placeholder))
            , onChange = valueChanged
            , label =
                Input.labelAbove (UI.Styles.getTypographicStyleFor UI.Styles.Label config)
                    (Element.column [ paddingXY 0 4 ]
                        [ Element.text label
                        ]
                    )
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


switch : Bool -> msg -> Config -> Element msg
switch model msg config =
    Element.el
        [ Element.inFront (switchHandle model config)
        , pointer
        , onClick msg
        ]
        (switchBody model config)


switchHandle : Bool -> Config -> Element msg
switchHandle model config =
    let
        position =
            if model == True then
                16

            else
                4
    in
    el
        [ Background.color (UI.Styles.color config).white
        , width (px 16)
        , Element.height (px 16)
        , Element.Border.rounded 12
        , Element.centerY
        , Element.moveRight position
        ]
        Element.none


switchBody : Bool -> Config -> Element msg
switchBody model config =
    let
        background =
            if model == True then
                (UI.Styles.color config).green500

            else
                (UI.Styles.color config).gray200
    in
    el
        [ Background.color background
        , width (px 36)
        , Element.height (px 24)
        , Element.Border.rounded 24
        ]
        Element.none


type Theme
    = Subtle
    | Clear
    | PrimaryLight
    | PrimaryDark


button : Theme -> String -> msg -> Config -> Element msg
button buttonTheme model msg config =
    let
        props =
            getButtonProps buttonTheme config
    in
    el
        (UI.Styles.getTypographicStyleFor UI.Styles.BodyBold config)
        (Input.button
            [ Background.color props.bgColor
            , paddingEach { top = 8, right = 12, bottom = 8, left = 12 }
            , Element.Border.rounded 6
            , Element.mouseOver <| [ Background.color props.hoverColor ]
            , Element.Font.color props.textColor
            ]
            { onPress = Just msg
            , label = text model
            }
        )


getButtonProps : Theme -> Config -> ButtonProps
getButtonProps buttonType config =
    case buttonType of
        Subtle ->
            { bgColor = (UI.Styles.color config).white
            , hoverColor = (UI.Styles.color config).gray200
            , textColor = (UI.Styles.color config).gray500
            }

        Clear ->
            { bgColor = (UI.Styles.color config).clear
            , hoverColor = (UI.Styles.color config).gray200
            , textColor = (UI.Styles.color config).gray500
            }

        PrimaryLight ->
            { bgColor = (UI.Styles.color config).primary100
            , hoverColor = (UI.Styles.color config).primary200
            , textColor = (UI.Styles.color config).primary500
            }

        PrimaryDark ->
            { bgColor = (UI.Styles.color config).primary400
            , hoverColor = (UI.Styles.color config).primary500
            , textColor = (UI.Styles.color config).primary100
            }


type alias ButtonProps =
    { bgColor : Color
    , hoverColor : Color
    , textColor : Color
    }


iconButton : Icon -> msg -> Config -> Element msg
iconButton icon msg config =
    el
        [ padding 8
        , Element.Border.rounded 6
        , pointer
        , Element.Events.onClick msg
        , Element.mouseOver [ Background.color (UI.Styles.color config).gray100 ]
        , Element.mouseDown [ Background.color (UI.Styles.color config).gray300 ]
        ]
        (buildIcon icon Outline)


chip : String -> RequestStatus -> Bool -> msg -> Config -> Element msg
chip text requestStatus saved msg config =
    el
        (UI.Styles.getTypographicStyleFor UI.Styles.Body config)
        (Element.row
            [ Background.color (UI.Styles.color config).white
            , paddingEach { top = 4, left = 12, bottom = 4, right = 4 }
            , Element.Border.rounded 6
            , Element.mouseOver <| [ Background.color (UI.Styles.color config).gray100 ]
            , Element.Border.color (UI.Styles.color config).gray200
            , Element.Border.width 1
            , spacing 4
            ]
            [ syncIndicator requestStatus (not saved) config
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


syncIndicator : RequestStatus -> Bool -> Config -> Element msg
syncIndicator status valueChanged config =
    if valueChanged then
        case status of
            NoRequest ->
                el
                    [ Background.color (UI.Styles.color config).primary200
                    , width (px 8)
                    , Element.height (px 8)
                    , Element.Border.rounded 12
                    , Element.centerY
                    ]
                    (text "")

            Fired ->
                el
                    [ Background.color (UI.Styles.color config).primary500
                    , width (px 8)
                    , Element.height (px 8)
                    , Element.Border.rounded 12
                    , Element.centerY
                    ]
                    (text "")

            Success ->
                el
                    [ Background.color (UI.Styles.color config).primary200
                    , width (px 8)
                    , Element.height (px 8)
                    , Element.Border.rounded 12
                    , Element.centerY
                    ]
                    (text "")

            Failed ->
                el
                    [ Background.color (UI.Styles.color config).green500
                    , width (px 8)
                    , Element.height (px 8)
                    , Element.Border.rounded 12
                    , Element.centerY
                    ]
                    (text "")

    else
        Element.none
