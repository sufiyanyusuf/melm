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
    | Label


type ColorScheme
    = Light
    | Dark


type alias Config =
    { scheme : ColorScheme
    }


type alias Colors =
    { primary500 : Color
    , primary400 : Color
    , primary300 : Color
    , primary200 : Color
    , primary100 : Color
    , gray500 : Color
    , gray400 : Color
    , gray300 : Color
    , gray200 : Color
    , gray100 : Color
    , green500 : Color
    , white : Color
    , clear : Color
    }


type alias ColorsHexValue =
    { primary500 : String
    , primary400 : String
    , primary300 : String
    , primary200 : String
    , primary100 : String
    , gray500 : String
    , gray400 : String
    , gray300 : String
    , gray200 : String
    , gray100 : String
    , green500 : String
    , white : String
    , clear : String
    }


colorHexValue : Config -> ColorsHexValue
colorHexValue config =
    case config.scheme of
        Light ->
            { primary500 = "#1C2890"
            , primary400 = "#4E5BCF"
            , primary300 = "#959EF0"
            , primary200 = "#B1B8FA"
            , primary100 = "#D3D8FF"
            , gray500 = "#F4F4F4"
            , gray400 = "#E1E1E1"
            , gray300 = "#808080"
            , gray200 = "#4C4C4C"
            , gray100 = "#1A1A1A"
            , white = "#FFFFFF"
            , green500 = "#11C74F"
            , clear = ""
            }

        Dark ->
            { primary500 = "#1C2890"
            , primary400 = "#4E5BCF"
            , primary300 = "#959EF0"
            , primary200 = "#B1B8FA"
            , primary100 = "#D3D8FF"
            , gray500 = "#F4F4F4"
            , gray400 = "#E1E1E1"
            , gray300 = "#808080"
            , gray200 = "#4C4C4C"
            , gray100 = "#1A1A1A"
            , white = "#FFFFFF"
            , green500 = "#11C74F"
            , clear = ""
            }


color : Config -> Colors
color config =
    case config.scheme of
        Light ->
            { primary500 = rgb255 28 40 144
            , primary400 = rgb255 78 91 207
            , primary300 = rgb255 149 158 240
            , primary200 = rgb255 177 184 250
            , primary100 = rgb255 211 216 255
            , gray500 = rgb255 26 26 26
            , gray400 = rgb255 76 76 76
            , gray300 = rgb255 128 128 128
            , gray200 = rgb255 225 225 225
            , gray100 = rgb255 244 244 244
            , white = rgb255 255 255 255
            , green500 = rgb255 17 199 112
            , clear = rgba 0 0 0 0
            }

        Dark ->
            { primary500 = rgb255 9 13 51
            , primary400 = rgb255 23 33 123
            , primary300 = rgb255 60 75 211
            , primary200 = rgb255 114 128 253
            , primary100 = rgb255 159 169 255
            , gray500 = rgb255 229 229 229
            , gray400 = rgb255 153 153 153
            , gray300 = rgb255 89 89 89
            , gray200 = rgb255 41 41 41
            , gray100 = rgb255 18 18 18
            , white = rgb255 25 25 25
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
            , Font.color (color config).gray400
            , Font.letterSpacing -0.6
            ]

        H2 ->
            [ Font.size 32
            , Font.bold
            , Font.family
                [ Font.typeface "Inter"
                , Font.sansSerif
                ]
            , Font.color (color config).gray400
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
            , Font.color (color config).gray400
            ]

        Body ->
            [ Font.size 18
            , Font.medium
            , Font.family
                [ Font.typeface "Inter"
                , Font.sansSerif
                ]
            , Font.color (color config).gray400
            , Font.letterSpacing -0.1
            ]

        Label ->
            [ Font.size 14
            , Font.medium
            , Font.family
                [ Font.typeface "Inter"
                , Font.sansSerif
                ]
            , Font.color (color config).gray300
            , Font.letterSpacing 0.2
            ]

        BodyBold ->
            [ Font.size 18
            , Font.semiBold
            , Font.family
                [ Font.typeface "Inter"
                , Font.sansSerif
                ]
            , Font.color (color config).gray400
            , Font.letterSpacing -0.1
            ]

        Code ->
            [ Font.size 14
            , Font.regular
            , Font.family
                [ Font.typeface "Inter"
                , Font.monospace
                ]
            , Font.color (color config).gray400
            ]

        CardTitle ->
            [ Font.size 24
            , Font.semiBold
            , Font.family
                [ Font.typeface "Inter"
                , Font.sansSerif
                ]
            , Font.letterSpacing -0.2
            , Font.color (color config).gray400
            ]
