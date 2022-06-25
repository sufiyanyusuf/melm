module UI.PageViews.Settings exposing (..)

import Element exposing (..)
import Request exposing (RequestStatus(..))
import UI.Components.Toolbar
import UI.Elements as Elements exposing (Theme(..), button, textfield)
import UI.Icons exposing (Icon(..))
import UI.Styles exposing (ColorScheme(..), Config)


type Msg
    = TokenValueChanged String
    | EndpointValueChanged String
    | Save
    | UpdateColorScheme ColorScheme
    | None


type alias Model =
    { tokenValue : String
    , colorScheme : UI.Styles.ColorScheme
    , savedTokenValue : String
    , endpointValue : String
    , savedEndpointValue : String
    }


init : Model
init =
    { tokenValue = "", savedTokenValue = "", colorScheme = UI.Styles.Light, endpointValue = "http://localhost:7700", savedEndpointValue = "http://localhost:7700" }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TokenValueChanged t ->
            ( { model | tokenValue = t }
            , Cmd.none
            )

        EndpointValueChanged t ->
            ( { model | endpointValue = t }
            , Cmd.none
            )

        UpdateColorScheme s ->
            ( { model | colorScheme = s }, Cmd.none )

        Save ->
            ( { model | savedTokenValue = model.tokenValue, savedEndpointValue = model.endpointValue }, Cmd.none )

        None ->
            ( model, Cmd.none )


valueChanged : Model -> Bool
valueChanged model =
    if model.tokenValue /= model.savedTokenValue || model.endpointValue /= model.savedEndpointValue then
        True

    else
        False


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
            , textfield model.endpointValue "Endpoint" "http://localhost:7700" EndpointValueChanged None None config
            , Elements.spacer UI.Styles.XL
            , textfield model.tokenValue "Token" "9438u093ty94y3989428ur929r20kfjvdfv7vfs" TokenValueChanged None None config
            , Elements.spacer UI.Styles.XL
            , Element.column []
                [ el (UI.Styles.getTypographicStyleFor UI.Styles.Label config) (Element.text "Theme")
                , Elements.spacer UI.Styles.SM
                , Element.row []
                    [ Elements.tile LightMode "Light" (model.colorScheme == Light) (UpdateColorScheme Light) config
                    , Elements.spacer UI.Styles.SM
                    , Elements.tile DarkMode "Dark" (model.colorScheme == Dark) (UpdateColorScheme Dark) config
                    , Elements.spacer UI.Styles.SM
                    , Elements.tile SystemThemeMode "System" False (UpdateColorScheme Dark) config
                    ]
                ]
            ]
        ]


toolbarView : Model -> Config -> Element Msg
toolbarView model config =
    let
        toolbarModel =
            { valueChanged = valueChanged model
            , loading = False
            , showCreateAction = False
            , title = "Settings"
            }
    in
    UI.Components.Toolbar.toolbarView toolbarModel None Save None config
