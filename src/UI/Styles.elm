module UI.Styles exposing
    ( ColorHue(..)
    , ColorIntensity(..)
    , ColorScheme(..)
    , Config
    , Size(..)
    , Typography(..)
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
                            rgb255 9 13 51

                        I200 ->
                            rgb255 23 33 123

                        I300 ->
                            rgb255 60 75 211

                        I400 ->
                            rgb255 114 128 253

                        _ ->
                            rgba255 159 169 255 1

                Grayscale ->
                    case intensity of
                        I100 ->
                            rgb255 18 18 18

                        I200 ->
                            rgb255 41 41 41

                        I300 ->
                            rgb255 89 89 89

                        I400 ->
                            rgb255 153 153 153

                        _ ->
                            rgb255 229 229 229

                Green ->
                    rgb255 17 199 112

                White ->
                    rgb255 25 25 25

                Clear ->
                    rgba 255 255 255 0



-- color : Config -> Palette
-- color config =
--     case config.scheme of
--         Light ->
--             { primary500 = rgb255 28 40 144
--             , primary400 = rgb255 78 91 207
--             , primary300 = rgb255 149 158 240
--             , primary200 = rgb255 177 184 250
--             , primary100 = rgb255 211 216 255
--             , gray500 = rgb255 26 26 26
--             , gray400 = rgb255 76 76 76
--             , gray300 = rgb255 128 128 128
--             , gray200 = rgb255 225 225 225
--             , gray100 = rgb255 244 244 244
--             , white = rgb255 255 255 255
--             , green500 = rgb255 17 199 112
--             , clear = rgba 0 0 0 0
--             }
--         Dark ->
--             { primary500 = rgb255 9 13 51
--             , primary400 = rgb255 23 33 123
--             , primary300 = rgb255 60 75 211
--             , primary200 = rgb255 114 128 253
--             , primary100 = rgb255 159 169 255
--             , gray500 = rgb255 229 229 229
--             , gray400 = rgb255 153 153 153
--             , gray300 = rgb255 89 89 89
--             , gray200 = rgb255 41 41 41
--             , gray100 = rgb255 18 18 18
--             , white = rgb255 25 25 25
--             , green500 = rgb255 17 199 112
--             , clear = rgba 0 0 0 0
--             }


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
            , Font.bold
            , Font.family
                [ Font.typeface "Inter"
                , Font.sansSerif
                ]
            , Font.color (color Grayscale I400 config)
            , Font.letterSpacing -0.6
            ]

        H2 ->
            [ Font.size 32
            , Font.bold
            , Font.family
                [ Font.typeface "Inter"
                , Font.sansSerif
                ]
            , Font.color (color Grayscale I400 config)
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
            , Font.color (color Grayscale I400 config)
            ]

        Body ->
            [ Font.size 18
            , Font.medium
            , Font.family
                [ Font.typeface "Inter"
                , Font.sansSerif
                ]
            , Font.color (color Grayscale I400 config)
            , Font.letterSpacing -0.1
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
            [ Font.size 18
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
            , Font.semiBold
            , Font.family
                [ Font.typeface "Inter"
                , Font.sansSerif
                ]
            , Font.letterSpacing -0.2
            , Font.color (color Grayscale I400 config)
            ]
