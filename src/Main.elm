module Main exposing (..)

import Api.Routes.Main exposing (..)
import Browser
import Dict exposing (Dict)
import Element exposing (..)
import Html exposing (Html)
import Http
import Request exposing (..)
import SweetPoll exposing (PollingState)
import UI.Components.SynonymCard exposing (Msg(..))
import UI.PageView as PageView exposing (Msg(..))
import UI.PageViews.Attributes as AttributesPage exposing (buildModelFromResponse)
import UI.PageViews.Documents as DocumentsPage
import UI.PageViews.Indexes as IndexesPage
import UI.PageViews.Settings as SettingsPage
import UI.PageViews.StopWords as StopWordsPage
import UI.PageViews.Synonyms as SynonymsPage exposing (Msg(..))
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
    | PollUpdate Task (SweetPoll.Msg String)
    | AddToPollQueue Task
    | UpdateKeysForIndex IndexKeys



-- Task id, indexUid


type Task
    = UpdateSynonymsTask Int String
    | UpdateAttributeTask Int String AttributesPage.AttributeType
    | UpdateStopWordsTask Int String


getTaskIndexUid : Task -> String
getTaskIndexUid task =
    case task of
        UpdateSynonymsTask _ uid ->
            uid

        UpdateAttributeTask _ uid _ ->
            uid

        UpdateStopWordsTask _ uid ->
            uid


getTaskId : Task -> Int
getTaskId t =
    case t of
        UpdateSynonymsTask tid _ ->
            tid

        UpdateAttributeTask tid _ _ ->
            tid

        UpdateStopWordsTask tid _ ->
            tid



-- MODEL


type alias Model =
    { token : Maybe String
    , savedToken : Maybe String
    , pages : Views.Model
    , indexes : List IndexesRouteResponseListItem -- Decouple this
    , documents : List String
    , documentKeys : ( String, List String )
    , selectedIndex : Maybe IndexesRouteResponseListItem -- Decouple this

    -- , stopWords : List StopWordsPage.StopWord
    , synonyms : List UI.Components.SynonymCard.Model -- Decouple this
    , pollingQueue : List ( Task, SweetPoll.PollingState String )
    , displayedAttrs : List AttributesPage.Attribute
    , sortableAttrs : List AttributesPage.Attribute
    , searchableAttrs : List AttributesPage.Attribute
    , filterableAttrs : List AttributesPage.Attribute
    , distinctAttr : List AttributesPage.Attribute --get rid of list, as is singular...
    , indexStats : Maybe IndexStats
    }



-- VIEW


view : Model -> Html Msg
view model =
    Element.layout [ width fill, height fill ]
        (Element.row
            [ width fill, height fill ]
            [ Sidebar.sidebarView (getSidebarViewModel model) |> Element.map SidebarMsg
            , PageView.view model.pages.selectedPage |> Element.map PageViewMsg
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

        AddToPollQueue task ->
            handlePollRequest model task

        UpdateKeysForIndex p ->
            let
                updatedAttributes =
                    AttributesPage.buildMockModelFromAttributes p.keys
            in
            -- update keys in model for index
            ( { model
                | documentKeys = ( p.indexUid, p.keys )
                , pages = updateAttributesViewModel model.pages updatedAttributes
              }
            , Cmd.none
            )



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

                        documentsPageViewModel =
                            DocumentsPage.Model documents
                    in
                    ( { model
                        | documents = documents
                        , pages = updateDocumentsViewModel model.pages documentsPageViewModel
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        HandleListStopWordsResponse r _ ->
            case r of
                Ok payload ->
                    let
                        stopWordsViewModel =
                            StopWordsPage.buildModelFromResponse payload model.pages.stopWords

                        updatedModelValue =
                            { model
                              -- | stopWords = stopWordsViewModel.words
                                | pages = updateStopWordsViewModel model.pages stopWordsViewModel
                            }
                    in
                    ( updatedModelValue, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        HandleUpdateSynonymsResponse r ->
            case r of
                Ok payload ->
                    update (AddToPollQueue (UpdateSynonymsTask payload.uid payload.indexUid)) model

                Err _ ->
                    ( model, Cmd.none )

        HandleListSynonymsResponse r indexUid ->
            case r of
                Ok payload ->
                    let
                        synonyms =
                            buildSynonymsViewModelFromApiResponse payload indexUid

                        updatedSynonymsViewModel =
                            { synonymStates = synonyms, indexUid = indexUid }
                    in
                    ( { model
                        | synonyms = synonyms
                        , pages = updateSynonymsViewModel model.pages updatedSynonymsViewModel
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        HandleIndexKeysResponse _ ->
            ( model
            , Api.Routes.Main.buildRequest
                (Api.Routes.Main.buildPayload (ListDisplayedAttrs "suggestions" Api.Routes.Main.maybeStringListDecoder))
                (Maybe.withDefault
                    ""
                    model.savedToken
                )
                |> Cmd.map ApiRequest
            )

        HandleListDisplayedAttrsResponse r _ ->
            case r of
                Ok payload ->
                    let
                        updatedViewModel =
                            buildModelFromResponse AttributesPage.Displayed
                                payload
                                (getAttributesViewModel model)
                    in
                    ( { model
                        | displayedAttrs = updatedViewModel.displayed
                        , pages = updateAttributesViewModel model.pages updatedViewModel
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        HandleListSortableAttrsResponse r _ ->
            case r of
                Ok payload ->
                    let
                        updatedViewModel =
                            buildModelFromResponse AttributesPage.Sortable
                                payload
                                (getAttributesViewModel model)
                    in
                    ( { model
                        | sortableAttrs = updatedViewModel.sortable
                        , pages = updateAttributesViewModel model.pages updatedViewModel
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        HandleListFilterableAttrsResponse r _ ->
            case r of
                Ok payload ->
                    let
                        updatedViewModel =
                            buildModelFromResponse AttributesPage.Filterable
                                payload
                                (getAttributesViewModel model)
                    in
                    ( { model
                        | filterableAttrs = updatedViewModel.filterable
                        , pages = updateAttributesViewModel model.pages updatedViewModel
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        HandleListSearchableAttrsResponse r _ ->
            case r of
                Ok payload ->
                    let
                        updatedViewModel =
                            buildModelFromResponse AttributesPage.Searchable
                                payload
                                (getAttributesViewModel model)
                    in
                    ( { model
                        | searchableAttrs = updatedViewModel.searchable
                        , pages = updateAttributesViewModel model.pages updatedViewModel
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        HandleListDistinctAttrResponse r _ ->
            case r of
                Ok payload ->
                    case payload of
                        Just p ->
                            let
                                updatedViewModel =
                                    buildModelFromResponse AttributesPage.Distinct
                                        [ p ]
                                        (getAttributesViewModel model)
                            in
                            ( { model
                                | distinctAttr = updatedViewModel.distinct
                                , pages = updateAttributesViewModel model.pages updatedViewModel
                              }
                            , Cmd.none
                            )

                        Nothing ->
                            ( model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        HandleUpdateDisplayedAttrsResponse r ->
            case r of
                Ok payload ->
                    update (AddToPollQueue (UpdateAttributeTask payload.uid payload.indexUid AttributesPage.Displayed)) model

                Err _ ->
                    ( model, Cmd.none )

        HandleUpdateSearchableAttrsResponse r ->
            case r of
                Ok payload ->
                    update (AddToPollQueue (UpdateAttributeTask payload.uid payload.indexUid AttributesPage.Searchable)) model

                Err _ ->
                    ( model, Cmd.none )

        HandleUpdateSortableAttrsResponse r ->
            case r of
                Ok payload ->
                    update (AddToPollQueue (UpdateAttributeTask payload.uid payload.indexUid AttributesPage.Sortable)) model

                Err _ ->
                    ( model, Cmd.none )

        HandleUpdateFilterableAttrsResponse r ->
            case r of
                Ok payload ->
                    update (AddToPollQueue (UpdateAttributeTask payload.uid payload.indexUid AttributesPage.Filterable)) model

                Err _ ->
                    ( model, Cmd.none )

        HandleUpdateDistinctAttrResponse r ->
            case r of
                Ok payload ->
                    update (AddToPollQueue (UpdateAttributeTask payload.uid payload.indexUid AttributesPage.Distinct)) model

                Err _ ->
                    ( model, Cmd.none )

        HandleUpdateStopWordsResponse r ->
            case r of
                Ok payload ->
                    update (AddToPollQueue (UpdateStopWordsTask payload.uid payload.indexUid)) model

                Err _ ->
                    ( model, Cmd.none )

        HandleStatsResponse r indexUid ->
            case r of
                Ok payload ->
                    let
                        keys =
                            Dict.keys payload.fieldDistribution

                        updatedAttributesPageViewModel =
                            AttributesPage.buildMockModelFromAttributes keys
                    in
                    ( { model
                        | indexStats = Just payload
                        , displayedAttrs = updatedAttributesPageViewModel.displayed
                        , sortableAttrs = updatedAttributesPageViewModel.sortable
                        , searchableAttrs = updatedAttributesPageViewModel.searchable
                        , filterableAttrs = updatedAttributesPageViewModel.filterable
                        , distinctAttr = updatedAttributesPageViewModel.distinct
                        , documentKeys = ( indexUid, keys )
                        , pages = updateAttributesViewModel model.pages updatedAttributesPageViewModel
                      }
                    , Cmd.batch
                        [ Api.Routes.Main.buildRequest
                            (Api.Routes.Main.buildPayload (ListDisplayedAttrs "suggestions" Api.Routes.Main.maybeStringListDecoder))
                            (Maybe.withDefault
                                ""
                                model.savedToken
                            )
                            |> Cmd.map ApiRequest
                        , Api.Routes.Main.buildRequest
                            (Api.Routes.Main.buildPayload (ListFilterableAttrs "suggestions" Api.Routes.Main.maybeStringListDecoder))
                            (Maybe.withDefault
                                ""
                                model.savedToken
                            )
                            |> Cmd.map ApiRequest
                        , Api.Routes.Main.buildRequest
                            (Api.Routes.Main.buildPayload (ListSortableAttrs "suggestions" Api.Routes.Main.maybeStringListDecoder))
                            (Maybe.withDefault
                                ""
                                model.savedToken
                            )
                            |> Cmd.map ApiRequest
                        , Api.Routes.Main.buildRequest
                            (Api.Routes.Main.buildPayload (ListSearchableAttrs "suggestions" Api.Routes.Main.maybeStringListDecoder))
                            (Maybe.withDefault
                                ""
                                model.savedToken
                            )
                            |> Cmd.map ApiRequest
                        , Api.Routes.Main.buildRequest
                            (Api.Routes.Main.buildPayload (ListDistinctAttr "suggestions" Api.Routes.Main.stringDecoder))
                            (Maybe.withDefault
                                ""
                                model.savedToken
                            )
                            |> Cmd.map ApiRequest
                        ]
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

        PageView.DocumentsViewMsg _ ->
            ( model, Cmd.none )

        PageView.StopWordsViewMsg m ->
            handleStopWordsViewMsg model m

        PageView.SynonymsViewMsg msg ->
            handleSynonymsViewMsg model msg

        PageView.AttributesViewMsg msg ->
            handleAttributesViewMsg model msg


handleAttributesViewMsg : Model -> AttributesPage.Msg -> ( Model, Cmd Msg )
handleAttributesViewMsg model msg =
    case msg of
        AttributesPage.X _ ->
            ( model, Cmd.none )

        AttributesPage.Toggle attr attrType ->
            case attrType of
                AttributesPage.Displayed ->
                    let
                        updatedDisplayAttrs =
                            List.map
                                (\x ->
                                    if x.title == attr.title then
                                        { x | enabled = not x.enabled }

                                    else
                                        x
                                )
                                model.displayedAttrs

                        updatedModel =
                            { model | displayedAttrs = updatedDisplayAttrs }
                    in
                    ( { updatedModel
                        | pages = updateAttributesViewModel model.pages (getAttributesViewModel updatedModel)
                      }
                    , Cmd.none
                    )

                AttributesPage.Searchable ->
                    let
                        updatedSearchableAttrs =
                            List.map
                                (\x ->
                                    if x.title == attr.title then
                                        { x | enabled = not x.enabled }

                                    else
                                        x
                                )
                                model.searchableAttrs

                        updatedModel =
                            { model | searchableAttrs = updatedSearchableAttrs }
                    in
                    ( { updatedModel
                        | pages = updateAttributesViewModel model.pages (getAttributesViewModel updatedModel)
                      }
                    , Cmd.none
                    )

                AttributesPage.Sortable ->
                    let
                        updatedSortableAttrs =
                            List.map
                                (\x ->
                                    if x.title == attr.title then
                                        { x | enabled = not x.enabled }

                                    else
                                        x
                                )
                                model.sortableAttrs

                        updatedModel =
                            { model | sortableAttrs = updatedSortableAttrs }
                    in
                    ( { updatedModel
                        | pages = updateAttributesViewModel model.pages (getAttributesViewModel updatedModel)
                      }
                    , Cmd.none
                    )

                AttributesPage.Filterable ->
                    let
                        updatedFilterableAttrs =
                            List.map
                                (\x ->
                                    if x.title == attr.title then
                                        { x | enabled = not x.enabled }

                                    else
                                        x
                                )
                                model.filterableAttrs

                        updatedModel =
                            { model | filterableAttrs = updatedFilterableAttrs }
                    in
                    ( { updatedModel
                        | pages = updateAttributesViewModel model.pages (getAttributesViewModel updatedModel)
                      }
                    , Cmd.none
                    )

                AttributesPage.Distinct ->
                    let
                        updatedDistinctAttr =
                            List.map
                                (\x ->
                                    if x.title == attr.title then
                                        { x | enabled = not x.enabled }

                                    else
                                        { x | enabled = False }
                                )
                                model.distinctAttr

                        updatedModel =
                            { model | distinctAttr = updatedDistinctAttr }
                    in
                    ( { updatedModel
                        | pages = updateAttributesViewModel model.pages (getAttributesViewModel updatedModel)
                      }
                    , Cmd.none
                    )

        AttributesPage.Save ->
            ( model
            , Cmd.batch
                [ Api.Routes.Main.buildRequest
                    (Api.Routes.Main.buildPayload
                        (UpdateDisplayedAttrs "suggestions"
                            (List.filter (\x -> x.enabled == True) model.displayedAttrs
                                |> List.map (\x -> x.title)
                            )
                            Api.Routes.Main.settingsUpdateDecoder
                        )
                    )
                    (Maybe.withDefault
                        ""
                        model.savedToken
                    )
                    |> Cmd.map ApiRequest
                , Api.Routes.Main.buildRequest
                    (Api.Routes.Main.buildPayload
                        (UpdateFilterableAttrs "suggestions"
                            (List.filter (\x -> x.enabled == True) model.filterableAttrs
                                |> List.map (\x -> x.title)
                            )
                            Api.Routes.Main.settingsUpdateDecoder
                        )
                    )
                    (Maybe.withDefault
                        ""
                        model.savedToken
                    )
                    |> Cmd.map ApiRequest
                , Api.Routes.Main.buildRequest
                    (Api.Routes.Main.buildPayload
                        (UpdateSearchableAttrs "suggestions"
                            (List.filter (\x -> x.enabled == True) model.searchableAttrs
                                |> List.map (\x -> x.title)
                            )
                            Api.Routes.Main.settingsUpdateDecoder
                        )
                    )
                    (Maybe.withDefault
                        ""
                        model.savedToken
                    )
                    |> Cmd.map ApiRequest
                , Api.Routes.Main.buildRequest
                    (Api.Routes.Main.buildPayload
                        (UpdateSortableAttrs "suggestions"
                            (List.filter (\x -> x.enabled == True) model.sortableAttrs
                                |> List.map (\x -> x.title)
                            )
                            Api.Routes.Main.settingsUpdateDecoder
                        )
                    )
                    (Maybe.withDefault
                        ""
                        model.savedToken
                    )
                    |> Cmd.map ApiRequest
                , Api.Routes.Main.buildRequest
                    (Api.Routes.Main.buildPayload
                        (UpdateDisplayedAttrs "suggestions"
                            (List.filter (\x -> x.enabled == True) model.displayedAttrs
                                |> List.map (\x -> x.title)
                            )
                            Api.Routes.Main.settingsUpdateDecoder
                        )
                    )
                    (Maybe.withDefault
                        ""
                        model.savedToken
                    )
                    |> Cmd.map ApiRequest
                , case AttributesPage.getDistinctAttr model.distinctAttr of
                    Just da ->
                        Api.Routes.Main.buildRequest
                            (Api.Routes.Main.buildPayload
                                (UpdateDistinctAttr "suggestions"
                                    da
                                    Api.Routes.Main.settingsUpdateDecoder
                                )
                            )
                            (Maybe.withDefault
                                ""
                                model.savedToken
                            )
                            |> Cmd.map ApiRequest

                    Nothing ->
                        Cmd.none
                ]
            )



-- ( model, Cmd.none )


handleSynonymsViewMsg : Model -> SynonymsPage.Msg -> ( Model, Cmd Msg )
handleSynonymsViewMsg model msg =
    case model.selectedIndex of
        Just i ->
            case msg of
                m ->
                    case m of
                        Sync ->
                            let
                                ( updatedSynonymsViewModel, _ ) =
                                    SynonymsPage.update msg (getSynonymsViewModel model i.uid)

                                currentSynonyms =
                                    List.map
                                        (\s -> ( s.synonymKey, s.synonymList ))
                                        model.synonyms
                                        |> List.filter (\( k, v ) -> k /= "" && v /= [])
                            in
                            ( { model
                                | pages = updateSynonymsViewModel model.pages updatedSynonymsViewModel
                                , synonyms = updatedSynonymsViewModel.synonymStates
                              }
                            , Api.Routes.Main.buildRequest
                                (Api.Routes.Main.buildPayload
                                    (UpdateSynonyms i.uid
                                        (Dict.fromList currentSynonyms)
                                        Api.Routes.Main.settingsUpdateDecoder
                                    )
                                )
                                (Maybe.withDefault
                                    ""
                                    model.savedToken
                                )
                                |> Cmd.map ApiRequest
                            )

                        _ ->
                            let
                                ( updatedSynonymsViewModel, _ ) =
                                    SynonymsPage.update msg (getSynonymsViewModel model i.uid)
                            in
                            ( { model
                                | pages = updateSynonymsViewModel model.pages updatedSynonymsViewModel
                                , synonyms = updatedSynonymsViewModel.synonymStates
                              }
                            , Cmd.none
                            )

        Nothing ->
            ( model, Cmd.none )


handleStopWordsViewMsg : Model -> StopWordsPage.Msg -> ( Model, Cmd Msg )
handleStopWordsViewMsg model msg =
    case msg of
        StopWordsPage.Sync ->
            ( model
            , Api.Routes.Main.buildRequest
                (Api.Routes.Main.buildPayload
                    (UpdateStopWords "suggestions"
                        (List.map (\x -> x.title) model.pages.stopWords.words)
                        Api.Routes.Main.settingsUpdateDecoder
                    )
                )
                (Maybe.withDefault
                    ""
                    model.savedToken
                )
                |> Cmd.map ApiRequest
            )

        _ ->
            let
                ( m, _ ) =
                    StopWordsPage.update msg model.pages.stopWords
            in
            ( { model | pages = updateStopWordsViewModel model.pages m }, Cmd.none )


handleSettingsViewMsg : Model -> SettingsPage.Msg -> ( Model, Cmd Msg )
handleSettingsViewMsg model msg =
    case msg of
        SettingsPage.KeyValueChanged t ->
            let
                updatedTokenValue =
                    { model | token = Just t }

                updatedSettingsPageViewModel =
                    getSettingsViewModel updatedTokenValue

                updatedModelValue =
                    { model
                        | token = updatedTokenValue.token
                        , pages = updateSettingsViewModel model.pages updatedSettingsPageViewModel
                    }
            in
            ( updatedModelValue
            , Cmd.none
            )

        SettingsPage.SaveKeyValue ->
            ( { model | savedToken = model.token }, Cmd.none )

        SettingsPage.None ->
            ( model, Cmd.none )


handleSidebarSelection : Model -> Sidebar.Msg -> ( Model, Cmd Msg )
handleSidebarSelection model sidebarMsg =
    case sidebarMsg of
        Sidebar.SelectPage p ->
            let
                pages =
                    model.pages

                updatedPages =
                    { pages | selectedPage = p }

                updatedModel =
                    { model | pages = updatedPages }
            in
            case p of
                Settings _ ->
                    ( updatedModel, Cmd.none )

                Documents _ ->
                    ( updatedModel
                    , Api.Routes.Main.buildRequest
                        (Api.Routes.Main.buildPayload (ListDocuments "suggestions"))
                        (Maybe.withDefault
                            ""
                            model.savedToken
                        )
                        |> Cmd.map ApiRequest
                    )

                Synonyms _ ->
                    ( updatedModel
                    , Api.Routes.Main.buildRequest
                        (Api.Routes.Main.buildPayload (ListSynonyms "suggestions" Api.Routes.Main.synonymsListDecoder))
                        (Maybe.withDefault
                            ""
                            model.savedToken
                        )
                        |> Cmd.map ApiRequest
                    )

                StopWords _ ->
                    ( updatedModel
                    , Api.Routes.Main.buildRequest
                        (Api.Routes.Main.buildPayload (ListStopWords "suggestions" Api.Routes.Main.maybeStringListDecoder))
                        (Maybe.withDefault
                            ""
                            model.savedToken
                        )
                        |> Cmd.map ApiRequest
                    )

                Attributes _ ->
                    ( updatedModel
                    , Api.Routes.Main.buildRequest
                        (Api.Routes.Main.buildPayload (Stats "suggestions" statsDecoder))
                        (Maybe.withDefault
                            ""
                            model.savedToken
                        )
                        |> Cmd.map ApiRequest
                    )

        _ ->
            ( model, Cmd.none )


handlePollRequest : Model -> Task -> ( Model, Cmd Msg )
handlePollRequest model task =
    let
        tasks =
            List.map (\( id, _ ) -> id) model.pollingQueue
    in
    if List.member task tasks then
        ( model, Cmd.none )

    else
        let
            taskId =
                getTaskId task

            ( pollState, pollCmd ) =
                SweetPoll.init (taskConfigBuilder taskId)
        in
        ( { model
            | pollingQueue = model.pollingQueue ++ [ ( task, pollState ) ]
          }
        , pollCmd |> Cmd.map (PollUpdate task)
        )



-- PORTS
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


receivedKeysForDocument : IndexKeys -> Msg
receivedKeysForDocument =
    UpdateKeysForIndex



-- VIEW MODEL GENERATORS


getSidebarViewModel : Model -> Sidebar.Model
getSidebarViewModel model =
    { pages = Views.getPageList model.pages
    , selectedPage = model.pages.selectedPage
    }


getSettingsViewModel : Model -> SettingsPage.Model
getSettingsViewModel model =
    { tokenValue = Maybe.withDefault "" model.token, title = "Settings" }


getIndexesViewModel : Model -> IndexesPage.Model
getIndexesViewModel model =
    { indexes = model.indexes }


getSynonymsViewModel : Model -> String -> SynonymsPage.Model
getSynonymsViewModel model indexUid =
    { synonymStates = model.synonyms, indexUid = indexUid }


getAttributesViewModel : Model -> AttributesPage.Model
getAttributesViewModel model =
    { displayed = model.displayedAttrs
    , sortable = model.sortableAttrs
    , searchable = model.searchableAttrs
    , filterable = model.filterableAttrs
    , distinct = model.distinctAttr
    }



-- VIEW MODEL SETTERS


updateSettingsViewModel : Views.Model -> SettingsPage.Model -> Views.Model
updateSettingsViewModel pages updatedPage =
    { pages | settings = updatedPage, selectedPage = Settings updatedPage }


updateDocumentsViewModel : Views.Model -> DocumentsPage.Model -> Views.Model
updateDocumentsViewModel pages updatedPage =
    { pages | documents = updatedPage, selectedPage = Documents updatedPage }


updateStopWordsViewModel : Views.Model -> StopWordsPage.Model -> Views.Model
updateStopWordsViewModel pages updatedPage =
    { pages | stopWords = updatedPage, selectedPage = StopWords updatedPage }


updateSynonymsViewModel : Views.Model -> SynonymsPage.Model -> Views.Model
updateSynonymsViewModel pages updatedPage =
    { pages | synonyms = updatedPage, selectedPage = Synonyms updatedPage }


updateAttributesViewModel : Views.Model -> AttributesPage.Model -> Views.Model
updateAttributesViewModel pages updatedPage =
    { pages | attributes = updatedPage, selectedPage = Attributes updatedPage }


handlePollUpdate : Model -> SweetPoll.Msg String -> Task -> ( Model, Cmd Msg )
handlePollUpdate model message task =
    let
        taskId =
            getTaskId task
    in
    let
        config =
            taskConfigBuilder taskId

        item =
            List.filter (\( a, _ ) -> a == task) model.pollingQueue
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
    -> Task
    -> ( Model, Cmd Msg )
handlePollSignal model newState newData error cmd task =
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
                                        (updatePollState task newState)
                                        model.pollingQueue
                              }
                            , cmd |> Cmd.map (PollUpdate task)
                            )

                        "processing" ->
                            case task of
                                UpdateSynonymsTask _ _ ->
                                    let
                                        updatedSynonyms =
                                            SynonymsPage.updateSyncStatusState model.synonyms Fired

                                        updatedSynonymsPageViewModel =
                                            { synonymStates = updatedSynonyms, indexUid = getTaskIndexUid task }
                                    in
                                    ( { model
                                        | pollingQueue =
                                            List.map
                                                (updatePollState task newState)
                                                model.pollingQueue
                                        , synonyms = updatedSynonyms
                                        , pages = updateSynonymsViewModel model.pages updatedSynonymsPageViewModel
                                      }
                                    , cmd |> Cmd.map (PollUpdate task)
                                    )

                                UpdateAttributeTask _ _ _ ->
                                    ( { model
                                        | pollingQueue =
                                            List.map
                                                (updatePollState task newState)
                                                model.pollingQueue
                                      }
                                    , cmd |> Cmd.map (PollUpdate task)
                                    )

                                UpdateStopWordsTask _ _ ->
                                    let
                                        updatedStopWordsViewModel =
                                            StopWordsPage.updateSyncStatusState model.pages.stopWords Fired
                                    in
                                    ( { model
                                        | pollingQueue =
                                            List.map
                                                (updatePollState task newState)
                                                model.pollingQueue

                                        -- , stopWords = updatedStopWordsViewModel.words
                                        , pages = updateStopWordsViewModel model.pages updatedStopWordsViewModel
                                      }
                                    , cmd |> Cmd.map (PollUpdate task)
                                    )

                        "succeeded" ->
                            case task of
                                UpdateSynonymsTask _ _ ->
                                    let
                                        updatedSynonyms =
                                            SynonymsPage.updateSyncStatusState model.synonyms Success

                                        updatedSynonymsPageViewModel =
                                            { synonymStates = updatedSynonyms, indexUid = getTaskIndexUid task }
                                    in
                                    ( { model
                                        | pollingQueue = List.filter (\( x, _ ) -> x /= task) model.pollingQueue
                                        , synonyms = updatedSynonyms
                                        , pages = updateSynonymsViewModel model.pages updatedSynonymsPageViewModel
                                      }
                                    , Cmd.none
                                    )

                                UpdateStopWordsTask _ _ ->
                                    let
                                        updatedStopWordsViewModel =
                                            StopWordsPage.updateSyncStatusState model.pages.stopWords Success
                                    in
                                    ( { model
                                        | pollingQueue = List.filter (\( x, _ ) -> x /= task) model.pollingQueue

                                        -- , stopWords = updatedStopWordsViewModel.words
                                        , pages = updateStopWordsViewModel model.pages updatedStopWordsViewModel
                                      }
                                    , Cmd.none
                                    )

                                UpdateAttributeTask _ _ attrType ->
                                    case attrType of
                                        AttributesPage.Displayed ->
                                            let
                                                updatedDisplayAttrs =
                                                    AttributesPage.updateSyncStatusState model.displayedAttrs Success

                                                updatedModel =
                                                    { model | displayedAttrs = updatedDisplayAttrs }
                                            in
                                            ( { updatedModel
                                                | pages = updateAttributesViewModel model.pages (getAttributesViewModel updatedModel)
                                              }
                                            , Cmd.none
                                            )

                                        AttributesPage.Searchable ->
                                            let
                                                updatedSearchableAttrs =
                                                    AttributesPage.updateSyncStatusState model.searchableAttrs Success

                                                updatedModel =
                                                    { model | searchableAttrs = updatedSearchableAttrs }
                                            in
                                            ( { updatedModel
                                                | pages = updateAttributesViewModel model.pages (getAttributesViewModel updatedModel)
                                              }
                                            , Cmd.none
                                            )

                                        AttributesPage.Sortable ->
                                            let
                                                updatedSortableAttrs =
                                                    AttributesPage.updateSyncStatusState model.sortableAttrs Success

                                                updatedModel =
                                                    { model | sortableAttrs = updatedSortableAttrs }
                                            in
                                            ( { updatedModel
                                                | pages = updateAttributesViewModel model.pages (getAttributesViewModel updatedModel)
                                              }
                                            , Cmd.none
                                            )

                                        AttributesPage.Filterable ->
                                            let
                                                updatedFilterableAttrs =
                                                    AttributesPage.updateSyncStatusState model.filterableAttrs Success

                                                updatedModel =
                                                    { model | filterableAttrs = updatedFilterableAttrs }
                                            in
                                            ( { updatedModel
                                                | pages = updateAttributesViewModel model.pages (getAttributesViewModel updatedModel)
                                              }
                                            , Cmd.none
                                            )

                                        AttributesPage.Distinct ->
                                            let
                                                updatedDistinctAttr =
                                                    AttributesPage.updateSyncStatusState model.distinctAttr Success

                                                updatedModel =
                                                    { model | distinctAttr = updatedDistinctAttr }
                                            in
                                            ( { updatedModel
                                                | pages = updateAttributesViewModel model.pages (getAttributesViewModel updatedModel)
                                              }
                                            , Cmd.none
                                            )

                        "failed" ->
                            case task of
                                UpdateSynonymsTask _ _ ->
                                    ( { model
                                        | pollingQueue = List.filter (\( x, _ ) -> x /= task) model.pollingQueue
                                        , synonyms = SynonymsPage.updateSyncStatusState model.synonyms Failed
                                      }
                                    , Cmd.none
                                    )

                                UpdateAttributeTask _ _ _ ->
                                    ( { model
                                        | pollingQueue = List.filter (\( x, _ ) -> x /= task) model.pollingQueue
                                        , displayedAttrs = AttributesPage.updateSyncStatusState model.displayedAttrs Failed
                                      }
                                    , Cmd.none
                                    )

                                UpdateStopWordsTask _ _ ->
                                    let
                                        updatedStopWordsViewModel =
                                            StopWordsPage.updateSyncStatusState model.pages.stopWords Failed
                                    in
                                    ( { model
                                        | pollingQueue = List.filter (\( x, _ ) -> x /= task) model.pollingQueue

                                        -- , stopWords = updatedStopWordsViewModel.words
                                        , pages = updateStopWordsViewModel model.pages updatedStopWordsViewModel
                                      }
                                    , Cmd.none
                                    )

                        _ ->
                            ( model, Cmd.none )

                Nothing ->
                    ( { model | pollingQueue = List.filter (\( x, _ ) -> x /= task) model.pollingQueue }
                    , Cmd.none
                    )


updatePollState : Task -> PollingState String -> (( Task, PollingState String ) -> ( Task, PollingState String ))
updatePollState task newState =
    \( t, s ) ->
        if t == task then
            ( t, newState )

        else
            ( t, s )


buildSynonymsViewModelFromApiResponse : Dict String (List String) -> String -> List UI.Components.SynonymCard.Model
buildSynonymsViewModelFromApiResponse d indexId =
    d
        |> Dict.toList
        |> List.indexedMap
            (\index ( title, values ) ->
                { index = index
                , synonymKey = title
                , synonymsValue = List.foldl (\x a -> x ++ "," ++ a) "" values |> String.dropRight 1
                , requestStatus = NoRequest
                , synonymList = values
                , taskId = Nothing
                , indexId = indexId
                , saved = Just ( title, values )
                }
            )


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            { token = Nothing
            , savedToken = Nothing
            , pages = Views.init "suggestions"
            , indexes = []
            , documents = []
            , selectedIndex =
                Just
                    { uid = "suggestions"
                    , name = "Suggestions"
                    , createdAt = ""
                    , updatedAt = ""
                    , primaryKey = "id"
                    }

            -- , stopWords = []
            , synonyms = (SynonymsPage.init "suggestions").synonymStates -- need to decouple ui from state
            , pollingQueue = []
            , documentKeys = ( "suggestions", [] )
            , displayedAttrs = []
            , sortableAttrs = []
            , searchableAttrs = []
            , filterableAttrs = []
            , distinctAttr = []
            , indexStats = Nothing
            }
    in
    ( model, Cmd.none )
