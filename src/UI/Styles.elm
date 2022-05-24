module UI.Styles exposing (..)

import Element exposing (..)
import Element.Font as Font


type Typography
    = H1
    | H2
    | H3
    | Body


type Size
    = SM
    | MD
    | LG
    | XL


color : { blue : Color, darkCharcoal : Color, lightBlue : Color, lightGrey : Color, white : Color }
color =
    { blue = rgb255 0x72 0x9F 0xCF
    , darkCharcoal = rgb255 0x2E 0x34 0x36
    , lightBlue = rgb255 0xC5 0xE8 0xF7
    , lightGrey = rgb255 0xE0 0xE0 0xE0
    , white = rgb255 0xFF 0xFF 0xFF
    }


getTypographicStyleFor : Typography -> List (Element.Attr () msg)
getTypographicStyleFor style =
    case style of
        H1 ->
            [ Font.size 40
            , Font.bold
            , Font.family
                [ Font.typeface "System-UI"
                , Font.sansSerif
                ]
            , width fill
            , Font.letterSpacing -1
            , Font.color color.darkCharcoal
            ]

        H2 ->
            [ Font.size 32
            , Font.bold
            , Font.family
                [ Font.typeface "System-UI"
                , Font.sansSerif
                ]
            , width fill
            , Font.letterSpacing -0.4
            , Font.color color.darkCharcoal
            ]

        H3 ->
            [ Font.size 24
            , Font.bold
            , Font.family
                [ Font.typeface "System-UI"
                , Font.sansSerif
                ]
            , width fill
            , Font.letterSpacing -0.2
            , Font.color color.darkCharcoal
            ]

        Body ->
            [ Font.size 18
            , Font.medium
            , Font.family
                [ Font.typeface "System-UI"
                , Font.sansSerif
                ]
            , width fill
            , Font.color color.darkCharcoal
            ]
