module UI.Components.SynonymCard exposing (..)

import Element exposing (..)
import Element.Background
import Element.Border exposing (rounded)
import Parser exposing (Step(..))
import Request exposing (..)
import Svg.Attributes exposing (radius)
import UI.Elements
import UI.Icons exposing (Icon(..), Style(..))
import UI.Styles exposing (ColorHue(..), ColorIntensity(..), Config)


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
    | Reset Int
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

        Reset i ->
            if model.index == i then
                ( resetValue model, Cmd.none )

            else
                ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


view : Model -> Config -> Element Msg
view model config =
    Element.column
        [ padding 24
        , Element.Background.color (UI.Styles.color White Generic config)
        , Element.Border.rounded 10
        , Element.width fill
        , spacing 4
        ]
        [ UI.Elements.textfield model.synonymKey "Title" "Tomato" (UpdatedTitle model.index) DoneEditing DoneEditing config
        , UI.Elements.spacer UI.Styles.XS
        , UI.Elements.textfield model.synonymsValue "Synonyms" "Tomayto, Tomaato, Tomaeto" (UpdatedList model.index) DoneEditing DoneEditing config
        , UI.Elements.spacer UI.Styles.SM
        , cardToolbar model config
        ]


cardToolbar : Model -> Config -> Element Msg
cardToolbar model config =
    Element.row
        [ width fill ]
        [ if model.requestStatus /= Fired then
            UI.Elements.iconButton Trash (Remove model.index) config

          else
            Element.none
        , Element.row []
            [ loadingView model config
            , failedView model config
            , valueChangedView model config
            ]
        ]


valueChangedView : Model -> Config -> Element Msg
valueChangedView model config =
    if valueChanged model && model.requestStatus /= Fired then
        Element.row
            [ Element.width Element.shrink
            , padding 12
            ]
            [ UI.Elements.button UI.Elements.Subtle "Cancel" (Reset model.index) config
            ]

    else
        Element.none


loadingView : Model -> Config -> Element Msg
loadingView model config =
    case model.requestStatus of
        Fired ->
            Element.row
                [ Element.width Element.shrink
                , paddingEach { top = 0, left = 8, bottom = 0, right = 0 }
                ]
                [ el
                    [ Element.Background.color (UI.Styles.color Primary I200 config)
                    , width (px 12)
                    , height (px 12)
                    , rounded 6
                    ]
                    (text "")
                , el
                    (UI.Styles.getTypographicStyleFor UI.Styles.Body config ++ [ padding 12 ])
                    (text "Syncing")
                ]

        _ ->
            Element.none


failedView : Model -> Config -> Element Msg
failedView model config =
    case model.requestStatus of
        Failed ->
            Element.row
                ([ padding 8
                 , Element.Background.color (UI.Styles.color White Generic config)
                 , Element.Border.rounded 8
                 , Element.width Element.fill
                 ]
                    ++ UI.Styles.getTypographicStyleFor UI.Styles.Body config
                )
                [ Element.el [ Element.width Element.fill, padding 4 ]
                    (text "Failed")
                , Element.row
                    [ Element.width Element.shrink
                    ]
                    [ UI.Elements.iconButton Retry (RetrySave model.index) config
                    , UI.Elements.iconButton Trash (Remove model.index) config
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
