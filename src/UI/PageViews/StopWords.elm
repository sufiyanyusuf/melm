module UI.PageViews.StopWords exposing (..)

import Element exposing (..)
import Request exposing (RequestStatus(..))
import UI.Components.Toolbar
import UI.Elements
import UI.Styles exposing (Config)


type Msg
    = Create
    | NewValueUpdated String
    | Remove StopWord
    | Sync
    | Reset
    | None


type alias Model =
    { words : List StopWord
    , newValue : String
    , deletionQueue : List StopWord
    }


type alias StopWord =
    { title : String
    , requestStatus : RequestStatus
    , saved : Bool
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


init : Model
init =
    { words = [], newValue = "", deletionQueue = [] }


view : Model -> Config -> Element Msg
view model config =
    Element.column
        [ height fill
        , width fill
        , paddingEach { top = 20, bottom = 12, left = 0, right = 0 }
        , inFront (toolbarView model config)
        ]
        [ Element.column
            [ width fill
            , height fill
            , scrollbarY
            , paddingXY 120 60
            ]
            [ UI.Elements.spacer UI.Styles.XL
            , UI.Elements.textfield model.newValue "Add a word" NewValueUpdated None Create config
            , UI.Elements.spacer UI.Styles.SM
            , Element.wrappedRow
                [ spacing 12
                ]
                (List.map (\w -> UI.Elements.chip w.title w.requestStatus w.saved (Remove w) config) model.words)
            , UI.Elements.spacer UI.Styles.XL
            ]
        ]


isLoading : Model -> Bool
isLoading model =
    (List.map (\x -> x.requestStatus) model.words
        |> List.filter (\x -> x == Fired)
        |> List.length
    )
        /= 0


getValueChanged : Model -> Bool
getValueChanged model =
    (List.map (\x -> x.saved) model.words
        |> List.filter (\x -> x /= True)
        |> List.length
    )
        /= 0
        || List.length model.deletionQueue
        > 0


toolbarView : Model -> Config -> Element Msg
toolbarView m config =
    let
        toolbarModel =
            { valueChanged = getValueChanged m
            , loading = isLoading m
            , showCreateAction = False
            , title = "Stopwords"
            }
    in
    UI.Components.Toolbar.toolbarView toolbarModel None Sync Reset config


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
        let
            nw =
                m.words
                    ++ [ createNew m.newValue ]
                    |> List.sortBy .title
        in
        { m | words = nw, newValue = "" }


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


updateSyncStatusState : Model -> RequestStatus -> Model
updateSyncStatusState model status =
    case status of
        Success ->
            let
                words =
                    List.map
                        (\c ->
                            if c.saved == False then
                                { c | requestStatus = status, saved = True }

                            else
                                c
                        )
                        model.words
            in
            { model | words = words }

        _ ->
            let
                words =
                    List.map
                        (\c ->
                            if c.saved == False then
                                { c | requestStatus = status }

                            else
                                c
                        )
                        model.words
            in
            { model | words = words }
