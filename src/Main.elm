module Main exposing (..)

import Api.Routes.Main exposing (..)
import Browser
import Element exposing (..)
import Html exposing (Html)
import UI.PageView as PageView exposing (Msg(..))
import UI.Pages as Views exposing (Page(..))
import UI.Pages.Settings exposing (Msg(..))
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
            { selectedPage = Views.Indexes
            , token = Nothing
            , savedToken = Nothing
            , pages = Views.init
            }
    in
    ( model, Cmd.none )



-- MODEL


type alias Model =
    { selectedPage : Views.Page
    , token : Maybe String
    , savedToken : Maybe String
    , pages : List Page
    }



-- VIEW


view : Model -> Html Msg
view model =
    Element.layout [ width fill, height fill, padding 20 ]
        (Element.row
            [ width fill, spacing 20, height fill ]
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

        PageView.DocumentsViewMsg _ ->
            Debug.todo "branch 'DocumentsViewMsg _' not implemented"

        PageView.KeysViewMsg _ ->
            Debug.todo "branch 'KeysViewMsg _' not implemented"

        PageView.TasksViewMsg _ ->
            Debug.todo "branch 'TasksViewMsg _' not implemented"


handleSettingsViewMsg : Model -> UI.Pages.Settings.Msg -> ( Model, Cmd Msg )
handleSettingsViewMsg model msg =
    case msg of
        X ->
            ( model, Cmd.none )

        KeyValueChanged t ->
            ( { model | token = Just t }, Cmd.none )

        SaveKeyValue ->
            ( { model | savedToken = model.token }, Cmd.none )



-- ( { model | token = Just t }, Cmd.none )


handleSidebarSelection : Model -> Sidebar.Msg -> ( Model, Cmd Msg )
handleSidebarSelection model sidebarMsg =
    let
        selectedPage =
            case sidebarMsg of
                Sidebar.SelectPage p ->
                    p
    in
    ( { model | selectedPage = selectedPage }, Cmd.none )


updateSidebarSelection : Sidebar.Model -> Page -> Sidebar.Model
updateSidebarSelection model page =
    { model | selectedPage = page }



-- MSG


type Msg
    = SidebarMsg Sidebar.Msg
    | PageViewMsg PageView.Msg



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
