module UI.PageViews.Synonyms exposing (..)

import Element exposing (..)
import Json.Decode exposing (Error(..))
import UI.Styles


type Msg
    = X


view : Element Msg
view =
    el
        (UI.Styles.getTypographicStyleFor UI.Styles.H1)
        (text "(Views.pageTitle Views.Tasks)")


type RequestStatus
    = None
    | Loading
    | Success
    | Failure


type alias CardModel =
    { status : RequestStatus
    , taskID : Maybe Int
    , title : String
    , values : List String
    }
