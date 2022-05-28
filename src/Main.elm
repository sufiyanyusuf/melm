module Main exposing (..)

import Api.Routes.Main exposing (..)
import Browser
import Element exposing (..)
import Html exposing (Html)
import UI.PageView as PageView exposing (Msg(..))
import UI.PageViews.Indexes
import UI.PageViews.Settings exposing (Msg(..))
import UI.Pages as Views exposing (Page(..))
import UI.Sidebar as Sidebar
import UI.Styles exposing (..)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            { selectedPage = Views.Indexes UI.PageViews.Indexes.init
            , token = Nothing
            , savedToken = Nothing
            , pages = Views.init
            , indexes = []
            , documents = []
            , selectedIndex = Nothing
            }
    in
    ( model, Cmd.none )



-- MODEL


type alias Model =
    { selectedPage : Views.Page
    , token : Maybe String
    , savedToken : Maybe String
    , pages : List Page
    , indexes : List IndexesRouteResponseListItem
    , documents : List String
    , selectedIndex : Maybe IndexesRouteResponseListItem
    }



-- VIEW


view : Model -> Html Msg
view model =
    Element.layout [ width fill, height fill ]
        (Element.row
            [ width fill, height fill ]
            [ Sidebar.sidebarView (getSidebarViewModel model) |> Element.map SidebarMsg
            , PageView.view model.selectedPage |> Element.map PageViewMsg
            ]
        )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SidebarMsg sidebarMsg ->
            handleSidebarSelection model sidebarMsg

        PageViewMsg pageViewMsg ->
            handlePageViewMessage model pageViewMsg

        ApiRequest r ->
            handleApiRequest model r



-- UPDATE HANDLERS


handleApiRequest : Model -> Api.Routes.Main.Msg -> ( Model, Cmd Msg )
handleApiRequest model apiResponse =
    case apiResponse of
        HandleListResponse r ->
            case r of
                Ok payload ->
                    let
                        updatedViewModel =
                            getIndexesViewModel { model | indexes = payload }
                    in
                    let
                        updatedIndexesPage =
                            Indexes updatedViewModel
                    in
                    let
                        updatedModelValue =
                            { model
                                | indexes = payload
                                , pages = updateIndexesViewModel model.pages updatedIndexesPage
                                , selectedPage = updatedIndexesPage
                            }
                    in
                    ( updatedModelValue
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        HandleShowResponse r ->
            case r of
                Ok payload ->
                    ( model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        HandleDocumentsResponse r ->
            case r of
                Ok payload ->
                    let
                        documents =
                            String.dropLeft 1 payload
                                |> String.dropRight 1
                                |> String.split "},"
                                |> List.map (\p -> p ++ "}")
                    in
                    let
                        updatedDocumentsPage =
                            Documents { documents = documents }
                    in
                    ( { model
                        | documents = documents
                        , pages = updateDocumentsViewModel model.pages updatedDocumentsPage
                        , selectedPage = updatedDocumentsPage
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )


handlePageViewMessage : Model -> PageView.Msg -> ( Model, Cmd Msg )
handlePageViewMessage model pageViewMsg =
    case pageViewMsg of
        PageView.SettingsViewMsg m ->
            handleSettingsViewMsg model m

        PageView.IndexesViewMsg _ ->
            Debug.todo "branch 'IndexesViewMsg _' not implemented"

        PageView.SearchViewMsg _ ->
            Debug.todo "branch 'SearchViewMsg _' not implemented"

        PageView.StatsViewMsg _ ->
            Debug.todo "branch 'StatsViewMsg _' not implemented"

        PageView.DocumentsViewMsg m ->
            ( model, Cmd.none )

        PageView.KeysViewMsg _ ->
            Debug.todo "branch 'KeysViewMsg _' not implemented"

        PageView.TasksViewMsg _ ->
            Debug.todo "branch 'TasksViewMsg _' not implemented"


handleSettingsViewMsg : Model -> UI.PageViews.Settings.Msg -> ( Model, Cmd Msg )
handleSettingsViewMsg model msg =
    case msg of
        X ->
            ( model, Cmd.none )

        KeyValueChanged t ->
            let
                updatedTokenValue =
                    { model | token = Just t }
            in
            let
                updatedSettingsPage =
                    Settings (getSettingsViewModel updatedTokenValue)
            in
            let
                updatedModelValue =
                    { model
                        | token = updatedTokenValue.token
                        , pages = updateSettingsViewModel model.pages updatedSettingsPage
                        , selectedPage = updatedSettingsPage
                    }
            in
            ( updatedModelValue
            , Cmd.none
            )

        SaveKeyValue ->
            ( { model | savedToken = model.token }, Cmd.none )


handleSidebarSelection : Model -> Sidebar.Msg -> ( Model, Cmd Msg )
handleSidebarSelection model sidebarMsg =
    let
        selectedPage =
            case sidebarMsg of
                Sidebar.SelectPage p ->
                    p
    in
    case sidebarMsg of
        Sidebar.SelectPage p ->
            case p of
                Indexes _ ->
                    ( { model | selectedPage = selectedPage }
                    , Api.Routes.Main.buildRequest
                        (Api.Routes.Main.buildPayload (List indexesRouteResponseListDecoder))
                        (Maybe.withDefault
                            ""
                            model.savedToken
                        )
                        |> Cmd.map ApiRequest
                    )

                Settings _ ->
                    ( { model | selectedPage = selectedPage }, Cmd.none )

                Search ->
                    ( { model | selectedPage = selectedPage }, Cmd.none )

                Stats ->
                    ( { model | selectedPage = selectedPage }, Cmd.none )

                Documents _ ->
                    ( { model | selectedPage = selectedPage }
                    , Api.Routes.Main.buildRequest
                        (Api.Routes.Main.buildPayload (ListDocuments "suggestions"))
                        (Maybe.withDefault
                            ""
                            model.savedToken
                        )
                        |> Cmd.map ApiRequest
                    )

                Keys ->
                    ( { model | selectedPage = selectedPage }, Cmd.none )

                Tasks ->
                    ( { model | selectedPage = selectedPage }, Cmd.none )



-- MSG


type Msg
    = SidebarMsg Sidebar.Msg
    | PageViewMsg PageView.Msg
    | ApiRequest Api.Routes.Main.Msg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch []



-- VIEW MODEL GENERATORS


getSidebarViewModel : Model -> Sidebar.Model
getSidebarViewModel model =
    { pages = model.pages
    , selectedPage = model.selectedPage
    }


getSettingsViewModel : Model -> UI.PageViews.Settings.Model
getSettingsViewModel model =
    { tokenValue = Maybe.withDefault "" model.token, title = "Settings" }


getIndexesViewModel : Model -> UI.PageViews.Indexes.Model
getIndexesViewModel model =
    { indexes = model.indexes }



-- getDocumentsViewModel : Model -> UI.PAGES
-- VIEW MODEL SETTERS


updateSettingsViewModel : List Page -> Page -> List Page
updateSettingsViewModel pages updatedPage =
    pages
        |> List.map
            (\p ->
                case p of
                    Settings _ ->
                        updatedPage

                    _ ->
                        p
            )


updateIndexesViewModel : List Page -> Page -> List Page
updateIndexesViewModel pages updatedPage =
    pages
        |> List.map
            (\p ->
                case p of
                    Indexes _ ->
                        updatedPage

                    _ ->
                        p
            )


updateDocumentsViewModel : List Page -> Page -> List Page
updateDocumentsViewModel pages updatedPage =
    pages
        |> List.map
            (\p ->
                case p of
                    Documents _ ->
                        updatedPage

                    _ ->
                        p
            )
