module UI.Elements exposing (button, chip, dropDown, iconButton, spacer, switch, syncIndicator, textfield)

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


dropDown : String -> List String -> msg -> msg -> Element msg
dropDown v l clickDropdownButton selectItem =
    column
        [ padding 8
        , moveDown 8

        -- , moveRight 8
        , Element.Border.shadow
            { offset = ( 0, 0 )
            , size = 0
            , blur = 15
            , color = Element.rgba255 186 189 182 0.6
            }
        , Background.color UI.Styles.color.white
        , Element.Border.rounded 8
        , Element.scrollbarY
        , Element.height
            (shrink
                |> minimum 120
                |> maximum 320
            )
        , Element.width fill
        ]
        [ dropDownButton v clickDropdownButton, dropDownBody l selectItem ]


dropDownButton : String -> msg -> Element msg
dropDownButton t msg =
    Element.row
        (List.concat
            [ [ Element.Events.onClick msg
              , width fill
              , padding 8
              , rounded 4
              , pointer
              , Element.mouseOver <| [ Background.color UI.Styles.color.gray300 ]
              ]

            -- , addIf isSelected <| Background.color UI.Styles.color.primary200
            ]
        )
        [ paragraph [ paddingEach { top = 0, left = 8, bottom = 0, right = 0 } ]
            [ el
                (UI.Styles.getTypographicStyleFor UI.Styles.Body)
                (text t)
            ]
        ]


dropDownBody : List String -> msg -> Element msg
dropDownBody l msg =
    Element.column
        [ width fill
        ]
        (List.map (\x -> dropDownButton x msg) l)
