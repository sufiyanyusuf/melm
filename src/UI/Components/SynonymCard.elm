module UI.Components.SynonymCard exposing (..)

import Element exposing (..)
import Element.Background
import Element.Border
import UI.Elements
import UI.Styles


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdatedTitle i t ->
            if model.index == i then
                ( { model | title = t }, Cmd.none )

            else
                ( model, Cmd.none )

        UpdatedList i l ->
            if model.index == i then
                ( { model | synonymList = l }, Cmd.none )

            else
                ( model, Cmd.none )

        Save _ ->
            ( { model | requestStatus = RequestedToSave }, Cmd.none )


type RequestStatus
    = None
    | RequestedToSave
    | SuccessfullySaved
    | FailedToSave


type alias Model =
    { index : Int
    , title : String
    , synonymList : String
    , requestStatus : RequestStatus
    }


type Msg
    = UpdatedTitle Int String
    | UpdatedList Int String
    | Save Model


cardView : Model -> Element Msg
cardView model =
    Element.column
        [ padding 8
        , Element.Background.color UI.Styles.color.white
        , Element.Border.rounded 8
        ]
        [ UI.Elements.textfield model.title "Tomato" (UpdatedTitle model.index)
        , UI.Elements.textfield model.synonymList "Tomayto, Tomaato, Tomaeto" (UpdatedList model.index)
        , loadingView model
        ]


loadingView : Model -> Element Msg
loadingView model =
    if model.requestStatus == RequestedToSave then
        el
            (UI.Styles.getTypographicStyleFor UI.Styles.Body ++ [ padding 12 ])
            (text "Saving")

    else
        Element.none


init : Int -> Model
init index =
    { index = index
    , title = ""
    , synonymList = ""
    , requestStatus = None
    }
