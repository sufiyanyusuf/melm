module UI.Components.SynonymCard exposing (..)

import Dict exposing (Dict)
import Element exposing (..)
import Element.Background
import Element.Border
import Svg.Attributes exposing (mode)
import UI.Elements
import UI.Icons exposing (Icon(..), Style(..))
import UI.Styles


type alias Model =
    { index : Int
    , synonymKey : String
    , synonymsValue : String
    , synonymList : List String
    , saved : Maybe ( String, List String )
    , requestStatus : RequestStatus
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
    | Reset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdatedTitle i t ->
            if model.index == i then
                ( { model | synonymKey = t }, Cmd.none )

            else
                ( model, Cmd.none )

        UpdatedList i l ->
            if model.index == i then
                ( { model | synonymsValue = l, synonymList = String.split "," l }, Cmd.none )

            else
                ( model, Cmd.none )

        Reset ->
            ( resetValue model, Cmd.none )

        _ ->
            ( model, Cmd.none )


type Request
    = Create Int
    | Update Int
    | Delete Int


type RequestStatus
    = None
    | Fired Request
    | Success
    | Failed Request


view : Model -> Element Msg
view model =
    Element.column
        [ padding 8
        , Element.Background.color UI.Styles.color.white
        , Element.Border.rounded 8
        , Element.width fill
        ]
        [ UI.Elements.textfield model.synonymKey "Tomato" (UpdatedTitle model.index) (DoneEditingTitle model.index)
        , UI.Elements.textfield model.synonymsValue "Tomayto, Tomaato, Tomaeto" (UpdatedList model.index) (DoneEditingList model)
        , loadingView model
        , failedView model
        ]


valueChangedView : Model -> Element Msg
valueChangedView model =
    if valueChanged model then
        Element.row
            [ Element.width Element.shrink
            , padding 12
            ]
            [ UI.Elements.button "Cancel" Reset
            ]

    else
        Element.none


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
                [ Element.el [ Element.width Element.fill, padding 4 ]
                    (text (failureString x))
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
    , synonymKey = ""
    , synonymsValue = ""
    , requestStatus = None
    , saved = Nothing
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


valueChanged : Model -> Bool
valueChanged model =
    case model.saved of
        Just ( k, v ) ->
            model.synonymKey /= k || List.sort model.synonymList /= List.sort v

        Nothing ->
            model.synonymKey /= "" && model.synonymList /= []


resetValue : Model -> Model
resetValue model =
    case model.saved of
        Just ( k, v ) ->
            { model | synonymKey = k, synonymList = v, synonymsValue = List.foldl (\x a -> x ++ "," ++ a) "" v }

        Nothing ->
            { model | synonymKey = "", synonymList = [], synonymsValue = "" }
