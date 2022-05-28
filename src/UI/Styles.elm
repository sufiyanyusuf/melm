module UI.Styles exposing (..)

import Element exposing (..)
import Element.Font as Font exposing (monospace)


type Typography
    = H1
    | H2
    | H3
    | Body
    | BodyBold
    | Code


type Size
    = SM
    | MD
    | LG
    | XL
    | FILL



-- color : { blue : Color, darkCharcoal : Color, lightBlue : Color, gray300 : Color, white : Color }


color : { primary500 : Color, gray500 : Color, primary100 : Color, gray300 : Color, gray100 : Color, white : Color }
color =
    { primary500 = rgb255 0x72 0x9F 0xCF
    , gray500 = rgb255 0x2E 0x34 0x36
    , primary100 = rgb255 234 235 245
    , gray300 = rgb255 0xE0 0xE0 0xE0
    , gray100 = rgb255 243 244 245
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
            , Font.letterSpacing -1
            , Font.color color.gray500
            ]

        H2 ->
            [ Font.size 32
            , Font.bold
            , Font.family
                [ Font.typeface "System-UI"
                , Font.sansSerif
                ]
            , Font.letterSpacing -0.4
            , Font.color color.gray500
            ]

        H3 ->
            [ Font.size 24
            , Font.bold
            , Font.family
                [ Font.typeface "System-UI"
                , Font.sansSerif
                ]
            , Font.letterSpacing -0.2
            , Font.color color.gray500
            ]

        Body ->
            [ Font.size 18
            , Font.medium
            , Font.family
                [ Font.typeface "System-UI"
                , Font.sansSerif
                ]
            , Font.color color.gray500
            ]

        BodyBold ->
            [ Font.size 18
            , Font.bold
            , Font.family
                [ Font.typeface "System-UI"
                , Font.sansSerif
                ]
            , Font.color color.gray500
            ]

        Code ->
            [ Font.size 14
            , Font.regular
            , Font.family
                [ Font.typeface "System-UI"
                , Font.monospace
                ]
            , Font.color color.gray500
            ]
