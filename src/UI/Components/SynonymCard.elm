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
                ( { model | synonymList = l }, Cmd.none )

            else
                ( model, Cmd.none )

        DoneEditingTitle i ->
            handleSave model i

        DoneEditingList i ->
            handleSave model i

        Save i ->
            ( model, Cmd.none )

        Remove i ->
            ( model, Cmd.none )


handleSave : Model -> Int -> ( Model, Cmd Msg )
handleSave model i =
    if model.index == i then
        if model.title /= "" && model.synonymList /= "" then
            ( { model | requestStatus = Fired Create }, Cmd.none )

        else
            ( model, Cmd.none )

    else
        ( model, Cmd.none )


type Request
    = Create
    | Update
    | Delete


type RequestStatus
    = None
    | Fired Request
    | Success
    | Failed Request


type alias Model =
    { index : Int
    , title : String
    , synonymList : String
    , requestStatus : RequestStatus
    }


type Msg
    = UpdatedTitle Int String
    | UpdatedList Int String
    | DoneEditingTitle Int
    | DoneEditingList Int
    | Save Int
    | Remove Int


cardView : Model -> Element Msg
cardView model =
    Element.column
        [ padding 8
        , Element.Background.color UI.Styles.color.white
        , Element.Border.rounded 8
        , Element.width fill
        ]
        [ UI.Elements.textfield model.title "Tomato" (UpdatedTitle model.index) (DoneEditingTitle model.index)
        , UI.Elements.textfield model.synonymList "Tomayto, Tomaato, Tomaeto" (UpdatedList model.index) (DoneEditingList model.index)
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
                    [ UI.Elements.iconButton Retry (Save model.index)
                    , UI.Elements.iconButton Trash (Remove model.index)
                    ]
                ]

        _ ->
            Element.none


init : Int -> Model
init index =
    { index = index
    , title = ""
    , synonymList = ""
    , requestStatus = None
    }


loadingString : Request -> String
loadingString r =
    case r of
        Create ->
            "Creating"

        Update ->
            "Updating"

        Delete ->
            "Deleting"


failureString : Request -> String
failureString r =
    case r of
        Create ->
            "Failed to create"

        Update ->
            "Failed to update"

        Delete ->
            "Failed to delete"
