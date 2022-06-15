module UI.PageViews.StopWords exposing (..)

import Element exposing (..)
import Request exposing (RequestStatus(..))
import UI.Elements
import UI.Styles


type Msg
    = Create
    | NewValueUpdated String
    | Remove StopWord
    | Sync
    | None


type alias Model =
    { words : List StopWord
    , newValue : String
    , deletionQueue : List StopWord
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Create ->
            ( addNewWord model, Cmd.none )

        NewValueUpdated v ->
            ( { model | newValue = v }, Cmd.none )

        Remove w ->
            ( removeWord model w, Cmd.none )

        _ ->
            ( model, Cmd.none )


type alias StopWord =
    { title : String
    , requestStatus : RequestStatus
    , saved : Bool
    }


init : Model
init =
    { words = [], newValue = "", deletionQueue = [] }


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
        , UI.Elements.textfield model.newValue "Add a word" NewValueUpdated None Create
        , UI.Elements.spacer UI.Styles.SM
        , Element.wrappedRow
            [ spacing 12
            , paddingEach { top = 20, bottom = 0, left = 0, right = 0 }
            ]
            (List.map (\w -> UI.Elements.chip w.title w.requestStatus w.saved (Remove w)) model.words)
        , UI.Elements.spacer UI.Styles.XL
        , toolbarView model
        ]


createNew : String -> StopWord
createNew s =
    { title = s, requestStatus = NoRequest, saved = False }


removeWord : Model -> StopWord -> Model
removeWord m w =
    { m | words = List.filter (\x -> x /= w) m.words, deletionQueue = m.deletionQueue ++ [ w ] }


addNewWord : Model -> Model
addNewWord m =
    let
        e : List String
        e =
            List.map (\x -> x.title) m.words
    in
    if List.member m.newValue e == True then
        m

    else
        { m | words = m.words ++ [ createNew m.newValue ], newValue = "" }


buildModelFromResponse : List String -> Model -> Model
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
    , newValue = m.newValue
    , deletionQueue = m.deletionQueue
    }


toolbarView : Model -> Element Msg
toolbarView _ =
    Element.row
        [ Element.width Element.shrink
        ]
        [ UI.Elements.button "Save" Sync
        ]
