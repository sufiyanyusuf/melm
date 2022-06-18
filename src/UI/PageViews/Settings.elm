module UI.PageViews.Settings exposing (..)

import Element exposing (..)
import Request exposing (RequestStatus(..))
import UI.Components.Toolbar
import UI.Elements as Elements exposing (Theme(..), button, textfield)
import UI.Styles exposing (ColorScheme(..), Config)


type Msg
    = TokenValueChanged String
    | SaveKeyValue
    | UpdateColorScheme ColorScheme
    | None


type alias Model =
    { tokenValue : String
    , colorScheme : UI.Styles.ColorScheme
    , savedTokenValue : String
    }


init : Model
init =
    { tokenValue = "", savedTokenValue = "", colorScheme = UI.Styles.Light }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TokenValueChanged t ->
            let
                updatedModel =
                    { model | tokenValue = t }
            in
            ( updatedModel
            , Cmd.none
            )

        UpdateColorScheme s ->
            ( { model | colorScheme = s }, Cmd.none )

        SaveKeyValue ->
            ( { model | savedTokenValue = model.tokenValue }, Cmd.none )

        None ->
            ( model, Cmd.none )


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
            [ Elements.spacer UI.Styles.XL
            , textfield model.tokenValue "Token" TokenValueChanged None None config
            , Elements.spacer UI.Styles.MD
            , Element.row []
                [ button Subtle "Light" (UpdateColorScheme Light) config
                , Elements.spacer UI.Styles.MD
                , button Subtle "Dark" (UpdateColorScheme Dark) config
                ]
            ]
        ]


toolbarView : Model -> Config -> Element Msg
toolbarView _ config =
    let
        toolbarModel =
            { valueChanged = False
            , loading = False
            , showCreateAction = False
            , title = "Settings"
            }
    in
    UI.Components.Toolbar.toolbarView toolbarModel None SaveKeyValue None config
