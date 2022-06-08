module Main exposing (..)

import Api.Helper exposing (rootUrl)
import Api.Routes.Main exposing (..)
import Browser
import Element exposing (..)
import Html exposing (Html)
import Http
import Json.Decode exposing (decodeValue)
import SweetPoll exposing (PollingState)
import UI.Components.SynonymCard exposing (Msg(..))
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



-- MSG


type Msg
    = SidebarMsg Sidebar.Msg
    | PageViewMsg PageView.Msg
    | ApiRequest Api.Routes.Main.Msg
    | PollUpdate Int (SweetPoll.Msg String)
    | AddToPollQueue Int



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

    -- , pollingState : Maybe (SweetPoll.PollingState String)
    , pollingQueue : List ( Int, SweetPoll.PollingState String )
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        -- config : SweetPoll.Config String
        -- config =
        --     SweetPoll.defaultConfig
        --         (Decode.field "fulldate" Decode.string)
        --         "https://script.google.com/macros/s/AKfycbyd5AcbAnWi2Yn0xhFRbyzS4qMq1VucMVgVvhul5XqS9HkAyJY/exec"
        -- ( initialPollingState, initialCmd ) =
        --     SweetPoll.init config
        model =
            { selectedPage = Views.Documents UI.PageViews.Documents.init
            , token = Nothing
            , savedToken = Nothing
            , pages = Views.init
            , indexes = []
            , documents = []
            , selectedIndex =
                Just
                    { uid = "suggestions"
                    , name = "suggestions"
                    , createdAt = ""
                    , updatedAt = ""
                    , primaryKey = "id"
                    }
            , stopWords = []
            , synonyms = UI.PageViews.Synonyms.init.synonymStates
            , pollingQueue = []
            }
    in
    ( model, Cmd.none )



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

        PollUpdate taskId m ->
            handlePollUpdate model m taskId

        AddToPollQueue taskId ->
            handlePollRequest model taskId



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

        HandleUpdateSynonymsResponse r ->
            case r of
                Ok payload ->
                    update (AddToPollQueue payload.uid) model

                Err _ ->
                    Debug.log "Errrror"
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
    case msg of
        m ->
            case m of
                CardViewMsg cm ->
                    case cm of
                        Save i ->
                            let
                                ( updatedSynonymsViewModel, _ ) =
                                    UI.PageViews.Synonyms.update msg (getSynonymsViewModel model)
                            in
                            Debug.log "Save ev in main"
                                ( model, Cmd.none )

                        DoneEditingList x ->
                            case model.selectedIndex of
                                Just i ->
                                    let
                                        ( updatedSynonymsViewModel, _ ) =
                                            UI.PageViews.Synonyms.update msg (getSynonymsViewModel model)
                                    in
                                    ( { model
                                        | pages = updateSynonymsViewModel model.pages (Synonyms updatedSynonymsViewModel)
                                        , selectedPage = Synonyms updatedSynonymsViewModel
                                        , synonyms = updatedSynonymsViewModel.synonymStates
                                      }
                                    , Api.Routes.Main.buildRequest
                                        (Api.Routes.Main.buildPayload (UpdateSynonyms i.uid ( x.title, x.synonymList ) Api.Routes.Main.settingsUpdateDecoder))
                                        (Maybe.withDefault
                                            ""
                                            model.savedToken
                                        )
                                        |> Cmd.map ApiRequest
                                    )

                                Nothing ->
                                    ( model, Cmd.none )

                        _ ->
                            let
                                ( updatedSynonymsViewModel, _ ) =
                                    UI.PageViews.Synonyms.update msg (getSynonymsViewModel model)
                            in
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


handlePollRequest : Model -> Int -> ( Model, Cmd Msg )
handlePollRequest model taskId =
    let
        taskIds =
            List.map (\( id, _ ) -> id) model.pollingQueue
    in
    if List.member taskId taskIds then
        ( model, Cmd.none )

    else
        let
            ( pollState, pollCmd ) =
                SweetPoll.init (taskConfigBuilder taskId)
        in
        ( { model
            | pollingQueue = model.pollingQueue ++ [ ( taskId, pollState ) ]
          }
        , pollCmd |> Cmd.map (PollUpdate taskId)
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


handlePollUpdate : Model -> SweetPoll.Msg String -> Int -> ( Model, Cmd Msg )
handlePollUpdate model message i =
    let
        config =
            taskConfigBuilder i

        item =
            List.filter (\( a, _ ) -> a == i) model.pollingQueue
                |> List.head
    in
    case item of
        Just ( id, pollingState ) ->
            case SweetPoll.update config message pollingState of
                { newState, newData, error, cmd } ->
                    handlePollSignal model newState newData error cmd id

        Nothing ->
            ( model, Cmd.none )


handlePollSignal :
    Model
    -> PollingState String
    -> Maybe String
    -> Maybe Http.Error
    -> Cmd (SweetPoll.Msg String)
    -> Int
    -> ( Model, Cmd Msg )
handlePollSignal model newState newData error cmd id =
    case error of
        Just _ ->
            ( model
            , Cmd.none
            )

        _ ->
            case newData of
                Just d ->
                    case d of
                        "enqueued" ->
                            ( { model
                                | pollingQueue =
                                    List.map
                                        (updatePollState id newState)
                                        model.pollingQueue
                              }
                            , cmd |> Cmd.map (PollUpdate id)
                            )

                        "processing" ->
                            ( { model
                                | pollingQueue =
                                    List.map
                                        (updatePollState id newState)
                                        model.pollingQueue
                              }
                            , cmd |> Cmd.map (PollUpdate id)
                            )

                        "succeeded" ->
                            ( { model | pollingQueue = List.filter (\( x, _ ) -> x /= id) model.pollingQueue }
                            , Cmd.none
                            )

                        "failed" ->
                            ( { model | pollingQueue = List.filter (\( x, _ ) -> x /= id) model.pollingQueue }
                            , Cmd.none
                            )

                        _ ->
                            ( model, Cmd.none )

                Nothing ->
                    ( { model | pollingQueue = List.filter (\( x, _ ) -> x /= id) model.pollingQueue }
                    , Cmd.none
                    )


updatePollState : Int -> PollingState String -> (( Int, PollingState String ) -> ( Int, PollingState String ))
updatePollState id newState =
    \( x, s ) ->
        if x == id then
            ( x, newState )

        else
            ( x, s )
