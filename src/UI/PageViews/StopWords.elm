module UI.PageViews.StopWords exposing (..)

import Element exposing (..)
import UI.Styles


type Msg
    = Add String
    | Remove Int


type alias Model =
    { words : List String
    }


init : Model
init =
    { words = [] }


view : Model -> Element Msg
view m =
    el
        (UI.Styles.getTypographicStyleFor UI.Styles.H1)
        (text "Stop Words")
