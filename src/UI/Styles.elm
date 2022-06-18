module UI.Styles exposing (..)

import Element exposing (..)
import Element.Font as Font


type Size
    = XS
    | SM
    | MD
    | LG
    | XL
    | FILL


type Typography
    = H1
    | H2
    | H3
    | Body
    | BodyBold
    | Code
    | CardTitle


type ColorScheme
    = Light
    | Dark


type alias Config =
    { scheme : ColorScheme
    }


type alias Colors =
    { primary500 : Color
    , primary300 : Color
    , primary200 : Color
    , primary100 : Color
    , gray500 : Color
    , gray300 : Color
    , gray200 : Color
    , gray100 : Color
    , green500 : Color
    , white : Color
    , clear : Color
    }


color : Config -> Colors
color config =
    case config.scheme of
        Light ->
            { primary500 = rgb255 0x72 0x9F 0xCF
            , primary300 = rgb255 218 221 246
            , primary200 = rgb255 218 221 246
            , primary100 = rgb255 234 235 245
            , gray500 = rgb255 0x2E 0x34 0x36
            , gray300 = rgb255 0xE0 0xE0 0xE0
            , gray200 = rgb255 243 244 245
            , gray100 = rgb255 243 244 245
            , white = rgb255 0xFF 0xFF 0xFF
            , green500 = rgb255 17 199 112
            , clear = rgba 0 0 0 0
            }

        Dark ->
            { primary500 = rgb255 0x72 0x9F 0xCF
            , primary300 = rgb255 218 221 246
            , primary200 = rgb255 218 221 246
            , primary100 = rgb255 234 235 245
            , gray500 = rgb255 0x2E 0x34 0x36
            , gray300 = rgb255 0xE0 0xE0 0xE0
            , gray200 = rgb255 243 244 245
            , gray100 = rgb255 243 244 245
            , white = rgb255 0xFF 0xFF 0xFF
            , green500 = rgb255 17 199 112
            , clear = rgba 0 0 0 0
            }


getTypographicStyleFor : Typography -> Config -> List (Element.Attr () msg)
getTypographicStyleFor style config =
    case style of
        H1 ->
            [ Font.size 40
            , Font.bold
            , Font.family
                [ Font.typeface "Inter"
                , Font.sansSerif
                ]
            , Font.color (color config).gray500
            , Font.letterSpacing -0.6
            ]

        H2 ->
            [ Font.size 32
            , Font.bold
            , Font.family
                [ Font.typeface "Inter"
                , Font.sansSerif
                ]
            , Font.color (color config).gray500
            , Font.letterSpacing -0.4
            ]

        H3 ->
            [ Font.size 24
            , Font.bold
            , Font.family
                [ Font.typeface "Inter"
                , Font.sansSerif
                ]
            , Font.letterSpacing -0.2
            , Font.color (color config).gray500
            ]

        Body ->
            [ Font.size 18
            , Font.medium
            , Font.family
                [ Font.typeface "Inter"
                , Font.sansSerif
                ]
            , Font.color (color config).gray500
            , Font.letterSpacing -0.1
            ]

        BodyBold ->
            [ Font.size 18
            , Font.bold
            , Font.family
                [ Font.typeface "Inter"
                , Font.sansSerif
                ]
            , Font.color (color config).gray500
            , Font.letterSpacing -0.1
            ]

        Code ->
            [ Font.size 14
            , Font.regular
            , Font.family
                [ Font.typeface "Inter"
                , Font.monospace
                ]
            , Font.color (color config).gray500
            ]

        CardTitle ->
            [ Font.size 24
            , Font.semiBold
            , Font.family
                [ Font.typeface "Inter"
                , Font.sansSerif
                ]
            , Font.letterSpacing -0.2
            , Font.color (color config).gray500
            ]
