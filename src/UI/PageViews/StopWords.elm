module UI.PageViews.StopWords exposing (..)

import Element exposing (..)
import Request exposing (RequestStatus(..))
import UI.Elements exposing (spacer)
import UI.Styles


type Msg
    = NewStopWord String
    | ValueUpdated String
    | Remove Int
    | Sync
    | None


type alias StopWord =
    { title : String
    , requestStatus : RequestStatus
    , saved : Bool
    }


type alias Model =
    { words : List StopWord
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
        , paddingEach { top = 0, bottom = 0, left = 0, right = 320 }
        ]
        [ el
            (UI.Styles.getTypographicStyleFor UI.Styles.H1)
            (text "Stop Words")
        , UI.Elements.spacer UI.Styles.XL
        , UI.Elements.textfield "" "Add a word" ValueUpdated None
        , UI.Elements.spacer UI.Styles.SM
        , Element.wrappedRow
            [ spacing 12
            , paddingEach { top = 20, bottom = 0, left = 0, right = 0 }
            ]
            (List.indexedMap (\i w -> UI.Elements.chip w (Remove i)) (List.map (\x -> x.title) model.words))
        ]


createNew : String -> StopWord
createNew s =
    { title = s, requestStatus = NoRequest, saved = False }


buildModelFromResponse : List String -> List StopWord -> Model
buildModelFromResponse r m =
    { words =
        List.map
            (\x ->
                { title = x
                , requestStatus = NoRequest
                , saved = True
                }
            )
            r
    }
