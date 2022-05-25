module UI.Pages.Search exposing (..)

import Element exposing (..)
import UI.Pages as Views exposing (Page)
import UI.Styles


type Msg
    = X


view : Element Msg
view =
    el
        (UI.Styles.getTypographicStyleFor UI.Styles.H1)
        (text "(Views.pageTitle Views.Search)")
