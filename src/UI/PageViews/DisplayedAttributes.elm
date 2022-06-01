module UI.PageViews.DisplayedAttributes exposing (..)

import Element exposing (..)
import UI.Styles


type Msg
    = X


view : Element Msg
view =
    el
        (UI.Styles.getTypographicStyleFor UI.Styles.H1)
        (text "(Views.pageTitle Views.Tasks)")
