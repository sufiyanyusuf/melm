module UI.Components.SynonymCard exposing (..)

import Element exposing (..)
import Element.Background
import Element.Border exposing (rounded)
import Parser exposing (Step(..))
import Request exposing (..)
import Svg.Attributes exposing (radius)
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
    }


type Msg
    = UpdatedTitle Int String
    | UpdatedList Int String
    | Remove Int
    | RetrySave Int
    | Save Int
    | Reset
    | DoneEditing


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



-- type Request
--     = Create
--     | Update
--     | Delete
-- type RequestStatus
--     = NoRequest
--     | Fired
--     | Success
--     | Failed


view : Model -> Element Msg
view model =
    Element.column
        [ padding 8
        , Element.Background.color UI.Styles.color.white
        , Element.Border.rounded 8
        , Element.width fill
        ]
        [ UI.Elements.textfield model.synonymKey "Tomato" (UpdatedTitle model.index) DoneEditing DoneEditing
        , UI.Elements.textfield model.synonymsValue "Tomayto, Tomaato, Tomaeto" (UpdatedList model.index) DoneEditing DoneEditing
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
            [ UI.Elements.button UI.Elements.Subtle "Cancel" Reset
            ]

    else
        Element.none


loadingView : Model -> Element Msg
loadingView model =
    case model.requestStatus of
        Fired ->
            Element.row
                [ Element.width Element.shrink
                , paddingEach { top = 0, left = 8, bottom = 0, right = 0 }
                ]
                [ el
                    [ Element.Background.color UI.Styles.color.primary200
                    , width (px 12)
                    , height (px 12)
                    , rounded 6
                    ]
                    (text "")
                , el
                    (UI.Styles.getTypographicStyleFor UI.Styles.Body ++ [ padding 12 ])
                    (text "Syncing")
                ]

        _ ->
            Element.none


failedView : Model -> Element Msg
failedView model =
    case model.requestStatus of
        Failed ->
            Element.row
                ([ padding 8
                 , Element.Background.color UI.Styles.color.white
                 , Element.Border.rounded 8
                 , Element.width Element.fill
                 ]
                    ++ UI.Styles.getTypographicStyleFor UI.Styles.Body
                )
                [ Element.el [ Element.width Element.fill, padding 4 ]
                    (text "Failed")
                , Element.row
                    [ Element.width Element.shrink
                    ]
                    [ UI.Elements.iconButton Retry (RetrySave model.index)
                    , UI.Elements.iconButton Trash (Remove model.index)
                    ]
                ]

        _ ->
            Element.none


init : Int -> Model
init index =
    { index = index
    , synonymKey = ""
    , synonymsValue = ""
    , requestStatus = NoRequest
    , saved = Nothing
    , synonymList = []
    , taskId = Nothing
    }


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
