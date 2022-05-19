module UI.Pages.Indexes exposing (..)

import Element exposing (..)
import UI.Styles
import UI.Views as Views exposing (Page)


type Msg
    = X


view : Element Msg
view =
    el
        (UI.Styles.getTypographicStyleFor UI.Styles.H1)
        (text (Views.pageTitle Views.Indexes))
