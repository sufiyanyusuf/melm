module Main exposing (..)

import Api.Routes.Main exposing (..)
import Browser
import Element exposing (..)
import Html exposing (Html)
import Http
import Json.Decode as Decode
import SweetPoll exposing (PollingState)
import UI.Components.SynonymCard
import UI.PageView as PageView exposing (Msg(..))
import UI.PageViews.Documents
import UI.PageViews.Indexes
import UI.PageViews.Settings exposing (Msg(..))
import UI.PageViews.StopWords
import UI.PageViews.Synonyms exposing (Msg(..))
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
        config : SweetPoll.Config String
        config =
            SweetPoll.defaultConfig
                (Decode.field "fulldate" Decode.string)
                "https://script.google.com/macros/s/AKfycbyd5AcbAnWi2Yn0xhFRbyzS4qMq1VucMVgVvhul5XqS9HkAyJY/exec"

        ( initialPollingState, initialCmd ) =
            SweetPoll.init config

        model =
            { selectedPage = Views.Documents UI.PageViews.Documents.init
            , token = Nothing
            , savedToken = Nothing
            , pages = Views.init
            , indexes = []
            , documents = []
            , selectedIndex = Nothing
            , stopWords = []
            , synonyms = UI.PageViews.Synonyms.init.synonymStates
            , pollingState = Nothing
            , requestQueue = []
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
    , stopWords : List String
    , synonyms : List UI.Components.SynonymCard.Model
    , pollingState : Maybe (SweetPoll.PollingState String)
    , requestQueue : List Int
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



-- MSG


type Msg
    = SidebarMsg Sidebar.Msg
    | PageViewMsg PageView.Msg
    | ApiRequest Api.Routes.Main.Msg
    | PollUpdate Int (SweetPoll.Config String) (SweetPoll.Msg String)



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

        PollUpdate i c m ->
            ( model, Cmd.none )



-- let
--     config : SweetPoll.Config String
--     config =
--         SweetPoll.defaultConfig
--             (Decode.field "fulldate" Decode.string)
--             "https://script.google.com/macros/s/AKfycbyd5AcbAnWi2Yn0xhFRbyzS4qMq1VucMVgVvhul5XqS9HkAyJY/exec"
-- in
-- case SweetPoll.update config x model.pollingState of
--     { newState, newData, error, cmd } ->
--         case newData of
--             Just y ->
--                 -- update model
--                 ( model, Cmd.none )
--             Nothing ->
--                 ( { model | pollingState = newState }
--                 , cmd |> Cmd.map PollUpdate
--                 )
-- UPDATE HANDLERS


handleApiRequest : Model -> Api.Routes.Main.Msg -> ( Model, Cmd Msg )
handleApiRequest model apiResponse =
    case apiResponse of
        HandleListResponse r ->
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

        HandleListStopWordsResponse r ->
            case r of
                Ok payload ->
                    let
                        updatedStopWordsPage =
                            StopWords { words = payload }
                    in
                    ( { model
                        | stopWords = payload
                        , pages = updateStopWordsViewModel model.pages updatedStopWordsPage
                        , selectedPage = updatedStopWordsPage
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

        PageView.TasksViewMsg _ ->
            Debug.todo "branch 'TasksViewMsg _' not implemented"

        PageView.StopWordsViewMsg _ ->
            ( model, Cmd.none )

        PageView.SynonymsViewMsg msg ->
            handleSynonymsViewMsg model msg


handleSynonymsViewMsg : Model -> UI.PageViews.Synonyms.Msg -> ( Model, Cmd Msg )
handleSynonymsViewMsg model msg =
    let
        ( updatedSynonymsViewModel, _ ) =
            UI.PageViews.Synonyms.update msg (getSynonymsViewModel model)
    in
    case msg of
        _ ->
            ( { model
                | pages = updateSynonymsViewModel model.pages (Synonyms updatedSynonymsViewModel)
                , selectedPage = Synonyms updatedSynonymsViewModel
                , synonyms = updatedSynonymsViewModel.synonymStates
              }
            , Cmd.none
            )


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

        None ->
            ( model, Cmd.none )


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
                Settings _ ->
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

                Tasks ->
                    ( { model | selectedPage = selectedPage }, Cmd.none )

                RankingRules ->
                    Debug.todo "branch 'RankingRules' not implemented"

                Synonyms _ ->
                    ( { model | selectedPage = selectedPage }, Cmd.none )

                StopWords _ ->
                    ( { model | selectedPage = selectedPage }
                    , Api.Routes.Main.buildRequest
                        (Api.Routes.Main.buildPayload (ListStopWords "suggestions" Api.Routes.Main.stopWordsListItemDecoder))
                        (Maybe.withDefault
                            ""
                            model.savedToken
                        )
                        |> Cmd.map ApiRequest
                    )



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


getStopWordsViewModel : Model -> UI.PageViews.StopWords.Model
getStopWordsViewModel model =
    { words = model.stopWords }


getSynonymsViewModel : Model -> UI.PageViews.Synonyms.Model
getSynonymsViewModel model =
    { synonymStates = model.synonyms }



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


updateStopWordsViewModel : List Page -> Page -> List Page
updateStopWordsViewModel pages updatedPage =
    pages
        |> List.map
            (\p ->
                case p of
                    StopWords _ ->
                        updatedPage

                    _ ->
                        p
            )


updateSynonymsViewModel : List Page -> Page -> List Page
updateSynonymsViewModel pages updatedPage =
    pages
        |> List.map
            (\p ->
                case p of
                    Synonyms _ ->
                        updatedPage

                    _ ->
                        p
            )


getPollingState : Model -> SweetPoll.Msg String -> SweetPoll.Config String -> Int -> ( Model, Cmd Msg )
getPollingState model message config i =
    let
        ( state, _ ) =
            SweetPoll.init config
    in
    case model.pollingState of
        Just s ->
            case SweetPoll.update config message s of
                { newState, newData, error, cmd } ->
                    handlePollSignal model newState newData error cmd config i

        Nothing ->
            case SweetPoll.update config message state of
                { newState, newData, error, cmd } ->
                    handlePollSignal model newState newData error cmd config i


handlePollSignal :
    Model
    -> PollingState String
    -> Maybe String
    -> Maybe Http.Error
    -> Cmd (SweetPoll.Msg String)
    -> SweetPoll.Config String
    -> Int
    -> ( Model, Cmd Msg )
handlePollSignal model newState newData error cmd config id =
    case error of
        Just _ ->
            ( { model | pollingState = Nothing }
            , Cmd.none
            )

        _ ->
            case newData of
                Just d ->
                    case d of
                        "enqueued" ->
                            ( { model | pollingState = Just newState }
                            , cmd |> Cmd.map (PollUpdate id config)
                            )

                        "processing" ->
                            ( { model | pollingState = Just newState }
                            , cmd |> Cmd.map (PollUpdate id config)
                            )

                        "succeeded" ->
                            ( { model | pollingState = Nothing }
                            , Cmd.none
                            )

                        "failed" ->
                            ( { model | pollingState = Nothing }
                            , Cmd.none
                            )

                        _ ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )



-- let
--     config : SweetPoll.Config String
--     config =
--         SweetPoll.defaultConfig
--             (Decode.field "fulldate" Decode.string)
--             "https://script.google.com/macros/s/AKfycbyd5AcbAnWi2Yn0xhFRbyzS4qMq1VucMVgVvhul5XqS9HkAyJY/exec"
-- in
-- case SweetPoll.update config x model.pollingState of
--     { newState, newData, error, cmd } ->
--         case newData of
--             Just y ->
--                 -- update model
--                 ( model, Cmd.none )
--             Nothing ->
--                 ( { model | pollingState = newState }
--                 , cmd |> Cmd.map PollUpdate
--                 )
