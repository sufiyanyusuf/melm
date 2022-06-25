module UI.Styles exposing
    ( ColorHue(..)
    , ColorIntensity(..)
    , ColorScheme(..)
    , Config
    , Size(..)
    , Typography(..)
    , applyFontColor
    , color
    , getTypographicStyleFor
    , hexColor
    )

import Element exposing (..)
import Element.Font as Font


type Size
    = XS
    | SM
    | MD
    | LG
    | XL
    | FILL


type alias Config =
    { scheme : ColorScheme
    }



-- Colors


type ColorScheme
    = Light
    | Dark


type ColorHue
    = Primary
    | Grayscale
    | Green
    | White
    | Clear


type ColorIntensity
    = I100
    | I200
    | I300
    | I400
    | I500
    | Generic


hexColor : ColorHue -> ColorIntensity -> Config -> String
hexColor hue intensity config =
    color hue intensity config
        |> convertToHex


color : ColorHue -> ColorIntensity -> Config -> Color
color hue intensity config =
    case config.scheme of
        Light ->
            case hue of
                Primary ->
                    case intensity of
                        I100 ->
                            rgb255 211 216 255

                        I200 ->
                            rgb255 177 184 250

                        I300 ->
                            rgb255 149 158 240

                        I400 ->
                            rgb255 78 91 207

                        _ ->
                            rgb255 28 40 144

                Grayscale ->
                    case intensity of
                        I100 ->
                            rgb255 244 244 244

                        I200 ->
                            rgb255 225 225 225

                        I300 ->
                            rgb255 128 128 128

                        I400 ->
                            rgb255 76 76 76

                        _ ->
                            rgb255 26 26 26

                Green ->
                    rgb255 17 199 112

                White ->
                    rgb255 255 255 255

                Clear ->
                    rgba 255 255 255 0

        Dark ->
            case hue of
                Primary ->
                    case intensity of
                        I100 ->
                            rgb255 39 44 94

                        I200 ->
                            rgb255 60 68 143

                        I300 ->
                            rgb255 78 88 180

                        I400 ->
                            rgb255 100 111 215

                        _ ->
                            rgb255 152 162 255

                Grayscale ->
                    case intensity of
                        I100 ->
                            rgb255 27 28 32

                        I200 ->
                            rgb255 44 45 51

                        I300 ->
                            rgb255 80 82 96

                        I400 ->
                            rgb255 137 141 159

                        _ ->
                            rgb255 229 229 229

                Green ->
                    rgb255 17 199 112

                White ->
                    rgb255 32 33 38

                Clear ->
                    rgba 255 255 255 0


convertToHex : Color -> String
convertToHex data =
    Element.toRgb data
        |> (\rgb ->
                "rgba("
                    ++ String.fromInt (ceiling (rgb.red * 255))
                    ++ ","
                    ++ String.fromInt (ceiling (rgb.green * 255))
                    ++ ","
                    ++ String.fromInt (ceiling (rgb.blue * 255))
                    ++ ","
                    ++ String.fromFloat rgb.alpha
                    ++ ")"
           )



-- Typography


type Typography
    = H1
    | H2
    | H3
    | Body
    | BodyBold
    | Code
    | CardTitle
    | Label


getTypographicStyleFor : Typography -> Config -> List (Element.Attr () msg)
getTypographicStyleFor style config =
    case style of
        H1 ->
            [ Font.size 40
            , Font.semiBold
            , Font.family
                [ Font.typeface "Inter"
                , Font.sansSerif
                ]
            , Font.color (color Grayscale I400 config)
            , Font.letterSpacing -0.6
            ]

        H2 ->
            [ Font.size 32
            , Font.semiBold
            , Font.family
                [ Font.typeface "Inter"
                , Font.sansSerif
                ]
            , Font.color (color Grayscale I400 config)
            , Font.letterSpacing -0.4
            ]

        H3 ->
            [ Font.size 24
            , Font.semiBold
            , Font.family
                [ Font.typeface "Inter"
                , Font.sansSerif
                ]
            , Font.letterSpacing -0.2
            , Font.color (color Grayscale I400 config)
            ]

        Body ->
            [ Font.size 16
            , Font.regular
            , Font.family
                [ Font.typeface "Inter"
                , Font.sansSerif
                ]
            , Font.color (color Grayscale I400 config)
            , Font.letterSpacing -0.08
            ]

        Label ->
            [ Font.size 14
            , Font.medium
            , Font.family
                [ Font.typeface "Inter"
                , Font.sansSerif
                ]
            , Font.color (color Grayscale I400 config)
            , Font.letterSpacing 0.2
            ]

        BodyBold ->
            [ Font.size 16
            , Font.semiBold
            , Font.family
                [ Font.typeface "Inter"
                , Font.sansSerif
                ]
            , Font.color (color Grayscale I400 config)
            , Font.letterSpacing -0.1
            ]

        Code ->
            [ Font.size 14
            , Font.regular
            , Font.family
                [ Font.typeface "Inter"
                , Font.monospace
                ]
            , Font.color (color Grayscale I400 config)
            ]

        CardTitle ->
            [ Font.size 24
            , Font.medium
            , Font.family
                [ Font.typeface "Inter"
                , Font.sansSerif
                ]
            , Font.letterSpacing -0.2
            , Font.color (color Grayscale I500 config)
            ]


applyFontColor : ColorHue -> ColorIntensity -> Config -> List (Element.Attr () msg) -> List (Element.Attr () msg)
applyFontColor hue intensity config attrs =
    attrs
        ++ [ Font.color (color hue intensity config) ]
