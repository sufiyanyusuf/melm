module UI.PageViews.Tasks exposing (..)

import Element exposing (..)
import UI.Styles exposing (Config)


type Msg
    = X


view : Config -> Element Msg
view config =
    el
        (UI.Styles.getTypographicStyleFor UI.Styles.H1 config)
        (text "(Views.pageTitle Views.Tasks)")
