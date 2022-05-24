module UI.Pages.Indexes exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Input as Input
import UI.Pages as Views exposing (Page)
import UI.Styles


type Msg
    = X


view : Element Msg
view =
    Element.column
        [ width fill
        , height fill
        , scrollbarY
        ]
        [ el
            (UI.Styles.getTypographicStyleFor UI.Styles.H1)
            (text (Views.pageTitle Views.Indexes))
        , Input.button
            [ Background.color UI.Styles.color.lightGrey
            , padding 20
            ]
            { onPress = Just X
            , label = text "My Button"
            }
        ]
