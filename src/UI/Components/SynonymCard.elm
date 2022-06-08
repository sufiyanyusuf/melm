module UI.Components.SynonymCard exposing (..)

import Element exposing (..)
import Element.Background
import Element.Border
import UI.Elements
import UI.Icons exposing (Icon(..), Style(..))
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
                ( { model | synonymsValue = l, synonymList = String.split "," l }, Cmd.none )

            else
                ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- DoneEditingTitle i ->
--     handleSave model i
-- DoneEditingList i ->
--     handleSave model i
-- Remove i ->
--     ( model, Cmd.none )
-- RetrySave i ->
--     ( model, Cmd.none )
-- Save i ->
--     Debug.log "Save event recv in card..."
--         ( model, Cmd.none )


type Request
    = Create Int
    | Update Int
    | Delete Int


type RequestStatus
    = None
    | Fired Request
    | Success
    | Failed Request


type alias Model =
    { index : Int
    , title : String
    , synonymsValue : String
    , requestStatus : RequestStatus
    , synonymList : List String
    , taskId : Maybe Int
    , indexId : String
    }


type Msg
    = UpdatedTitle Int String
    | UpdatedList Int String
    | DoneEditingTitle Int
    | DoneEditingList Model
    | Remove Int
    | RetrySave Int
    | Save Int


cardView : Model -> Element Msg
cardView model =
    Element.column
        [ padding 8
        , Element.Background.color UI.Styles.color.white
        , Element.Border.rounded 8
        , Element.width fill
        ]
        [ UI.Elements.textfield model.title "Tomato" (UpdatedTitle model.index) (DoneEditingTitle model.index)
        , UI.Elements.textfield model.synonymsValue "Tomayto, Tomaato, Tomaeto" (UpdatedList model.index) (DoneEditingList model)
        , loadingView model
        , failedView model
        ]


loadingView : Model -> Element Msg
loadingView model =
    case model.requestStatus of
        Fired x ->
            el
                (UI.Styles.getTypographicStyleFor UI.Styles.Body ++ [ padding 12 ])
                (text (loadingString x))

        _ ->
            Element.none


failedView : Model -> Element Msg
failedView model =
    case model.requestStatus of
        Failed x ->
            Element.row
                ([ padding 8
                 , Element.Background.color UI.Styles.color.white
                 , Element.Border.rounded 8
                 , Element.width Element.fill
                 ]
                    ++ UI.Styles.getTypographicStyleFor UI.Styles.Body
                )
                [ Element.el [ Element.width Element.fill, padding 4 ] (text (failureString x))
                , Element.row
                    [ Element.width Element.shrink
                    ]
                    [ UI.Elements.iconButton Retry (RetrySave model.index)
                    , UI.Elements.iconButton Trash (Remove model.index)
                    ]
                ]

        _ ->
            Element.none


init : Int -> String -> Model
init index indexUid =
    { index = index
    , title = ""
    , synonymsValue = ""
    , requestStatus = None
    , synonymList = []
    , taskId = Nothing
    , indexId = indexUid
    }


loadingString : Request -> String
loadingString r =
    case r of
        Create i ->
            "Creating"

        Update i ->
            "Updating"

        Delete i ->
            "Deleting"


failureString : Request -> String
failureString r =
    case r of
        Create i ->
            "Failed to create"

        Update i ->
            "Failed to update"

        Delete i ->
            "Failed to delete"
