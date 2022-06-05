module UI.PageViews.StopWords exposing (..)

import Element exposing (..)
import UI.Elements exposing (spacer)
import UI.Styles


type Msg
    = NewStopWord String
    | Remove Int
    | None


type alias Model =
    { words : List String
    }


init : Model
init =
    { words = [] }


view : Model -> Element Msg
view model =
    Element.column
        [ height fill
        , width fill
        , scrollbarY
        , padding 4
        ]
        [ el
            (UI.Styles.getTypographicStyleFor UI.Styles.H1)
            (text "Stop Words")
        , UI.Elements.spacer UI.Styles.XL
        , UI.Elements.textfield "" "Add a word" NewStopWord None
        , UI.Elements.spacer UI.Styles.SM
        , Element.wrappedRow
            [ spacing 12
            , paddingEach { top = 20, bottom = 0, left = 0, right = 320 }
            ]
            (List.indexedMap (\i w -> UI.Elements.chip w (Remove i)) model.words)
        ]
