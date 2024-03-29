module Main exposing (..)

import Api.Routes.Main exposing (..)
import Browser
import Dict exposing (Dict)
import Element exposing (..)
import Html exposing (Html)
import Http
import Request exposing (..)
import SweetPoll exposing (PollingState)
import UI.Components.Dropdown exposing (Msg(..))
import UI.Components.SynonymCard exposing (Msg(..))
import UI.PageView as PageView exposing (Msg(..))
import UI.PageViews.Attributes as AttributesPage
import UI.PageViews.Documents as DocumentsPage
import UI.PageViews.Settings as SettingsPage
import UI.PageViews.StopWords as StopWordsPage
import UI.PageViews.Synonyms as SynonymsPage
import UI.Pages as Views exposing (Page(..))
import UI.Sidebar as Sidebar exposing (Msg(..))
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
    { pages : Views.Model
    , pollingQueue : List ( Task, SweetPoll.PollingState String )
    , sidebarModel : Sidebar.Model
    }



-- VIEW


view : Model -> Html Msg
view model =
    let
        config =
            getConfig model
    in
    Element.layoutWith
        { options =
            [ focusStyle
                { borderColor = Just (UI.Styles.color Primary I300 config)
                , backgroundColor = Just (UI.Styles.color White Generic config)
                , shadow =
                    Just
                        { color = UI.Styles.color Primary I300 config
                        , offset = ( 0, 0 )
                        , blur = 0
                        , size = 2
                        }
                }
            ]
        }
        [ width fill, height fill ]
        (Element.row
            [ width fill, height fill ]
            [ Sidebar.sidebarView (getSidebarViewModel model) config |> Element.map SidebarMsg
            , PageView.view model.pages.selectedPage config |> Element.map PageViewMsg
            ]
        )


getConfig : Model -> Config
getConfig model =
    { scheme = model.pages.settings.colorScheme
    }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SidebarMsg sidebarMsg ->
            handleSidebarSelection model sidebarMsg

        PageViewMsg pageViewMsg ->
            handlePageViewMessage model pageViewMsg

        ApiRequest r ->
            handleApiResponse model r

        PollUpdate taskId m ->
            handlePollUpdate model m taskId

        AddToPollQueue task ->
            handlePollRequest model task

        UpdateKeysForIndex p ->
            let
                updatedAttributes =
                    AttributesPage.seedModelFromAttributeKeys p.keys
            in
            -- update keys in model for index
            ( { model
                -- | documentKeys = ( p.indexUid, p.keys )
                | pages = updateAttributesViewModel model.pages updatedAttributes
              }
            , Cmd.none
            )



-- UPDATE HANDLERS


handleApiResponse : Model -> Api.Routes.Main.Msg -> ( Model, Cmd Msg )
handleApiResponse model apiResponse =
    case apiResponse of
        HandleListIndexesResponse r ->
            case r of
                Ok payload ->
                    let
                        d =
                            model.sidebarModel.dropDown

                        s =
                            model.sidebarModel

                        selectedPage =
                            model.pages.selectedPage

                        u =
                            { model
                                | sidebarModel =
                                    { s
                                        | dropDown =
                                            let
                                                options =
                                                    List.map (\x -> { id = x.uid, title = x.name }) payload

                                                selectedValue =
                                                    if d.selectedValue == Nothing then
                                                        List.head options

                                                    else
                                                        d.selectedValue
                                            in
                                            { d
                                                | options = options
                                                , selectedValue = selectedValue
                                            }
                                    }
                            }
                    in
                    handleSidebarSelection u (Sidebar.SelectPage selectedPage)

                Err _ ->
                    let
                        purgedModel =
                            purgeModel model

                        selectedPage =
                            model.pages.selectedPage
                    in
                    handleSidebarSelection purgedModel (Sidebar.SelectPage selectedPage)

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
                        | pages = updateDocumentsViewModel model.pages documentsPageViewModel
                      }
                    , Cmd.none
                    )

                Err _ ->
                    let
                        documents =
                            []

                        documentsPageViewModel =
                            DocumentsPage.Model documents
                    in
                    ( { model
                        | pages = updateDocumentsViewModel model.pages documentsPageViewModel
                      }
                    , Cmd.none
                    )

        HandleListStopWordsResponse r _ ->
            case r of
                Ok payload ->
                    let
                        stopWordsViewModel =
                            StopWordsPage.buildModelFromResponse payload model.pages.stopWords

                        updatedModelValue =
                            { model
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

        HandleListSynonymsResponse r _ ->
            case r of
                Ok payload ->
                    let
                        synonyms =
                            buildSynonymsViewModelFromApiResponse payload

                        updatedSynonymsViewModel =
                            { synonymCards = synonyms, deletionQueue = model.pages.synonyms.deletionQueue }
                    in
                    ( { model
                        | pages = updateSynonymsViewModel model.pages updatedSynonymsViewModel
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        HandleIndexKeysResponse r ->
            ( model
            , Api.Routes.Main.buildRequest
                (Api.Routes.Main.buildPayload (ListDisplayedAttrs r.indexUid Api.Routes.Main.maybeStringListDecoder) (getRootUrl model))
                (getSavedToken model)
                |> Cmd.map ApiRequest
            )

        HandleListDisplayedAttrsResponse r _ ->
            case r of
                Ok payload ->
                    let
                        updatedAttrViewModel =
                            AttributesPage.update
                                (AttributesPage.HandleResponse AttributesPage.Displayed payload)
                                model.pages.attributes
                    in
                    ( { model
                        | pages = updateAttributesViewModel model.pages updatedAttrViewModel
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        HandleListSortableAttrsResponse r _ ->
            case r of
                Ok payload ->
                    let
                        updatedAttrViewModel =
                            AttributesPage.update
                                (AttributesPage.HandleResponse AttributesPage.Sortable payload)
                                model.pages.attributes
                    in
                    ( { model
                        | pages = updateAttributesViewModel model.pages updatedAttrViewModel
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        HandleListFilterableAttrsResponse r _ ->
            case r of
                Ok payload ->
                    let
                        updatedAttrViewModel =
                            AttributesPage.update
                                (AttributesPage.HandleResponse AttributesPage.Filterable payload)
                                model.pages.attributes
                    in
                    ( { model
                        | pages = updateAttributesViewModel model.pages updatedAttrViewModel
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        HandleListSearchableAttrsResponse r _ ->
            case r of
                Ok payload ->
                    let
                        updatedAttrViewModel =
                            AttributesPage.update
                                (AttributesPage.HandleResponse AttributesPage.Searchable payload)
                                model.pages.attributes
                    in
                    ( { model
                        | pages = updateAttributesViewModel model.pages updatedAttrViewModel
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
                                updatedAttrViewModel =
                                    AttributesPage.update
                                        (AttributesPage.HandleResponse AttributesPage.Distinct [ p ])
                                        model.pages.attributes
                            in
                            ( { model
                                | pages = updateAttributesViewModel model.pages updatedAttrViewModel
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
                            AttributesPage.seedModelFromAttributeKeys keys
                    in
                    ( { model
                        | pages = updateAttributesViewModel model.pages updatedAttributesPageViewModel
                      }
                    , let
                        rootUrl =
                            getRootUrl model
                      in
                      Cmd.batch
                        [ Api.Routes.Main.buildRequest
                            (Api.Routes.Main.buildPayload (ListDisplayedAttrs indexUid Api.Routes.Main.maybeStringListDecoder) rootUrl)
                            (getSavedToken model)
                            |> Cmd.map ApiRequest
                        , Api.Routes.Main.buildRequest
                            (Api.Routes.Main.buildPayload (ListFilterableAttrs indexUid Api.Routes.Main.maybeStringListDecoder) rootUrl)
                            (getSavedToken model)
                            |> Cmd.map ApiRequest
                        , Api.Routes.Main.buildRequest
                            (Api.Routes.Main.buildPayload (ListSortableAttrs indexUid Api.Routes.Main.maybeStringListDecoder) rootUrl)
                            (getSavedToken model)
                            |> Cmd.map ApiRequest
                        , Api.Routes.Main.buildRequest
                            (Api.Routes.Main.buildPayload (ListSearchableAttrs indexUid Api.Routes.Main.maybeStringListDecoder) rootUrl)
                            (getSavedToken model)
                            |> Cmd.map ApiRequest
                        , Api.Routes.Main.buildRequest
                            (Api.Routes.Main.buildPayload (ListDistinctAttr indexUid Api.Routes.Main.stringDecoder) rootUrl)
                            (getSavedToken model)
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
    let
        rootUrl =
            getRootUrl model
    in
    case msg of
        AttributesPage.Save ->
            case getCurrentlySelectedIndexId model of
                Just uid ->
                    ( model
                    , Cmd.batch
                        [ Api.Routes.Main.buildRequest
                            (Api.Routes.Main.buildPayload
                                (UpdateDisplayedAttrs uid
                                    (List.filter (\x -> x.enabled == True) model.pages.attributes.displayed
                                        |> List.map (\x -> x.title)
                                    )
                                    Api.Routes.Main.settingsUpdateDecoder
                                )
                                rootUrl
                            )
                            (getSavedToken model)
                            |> Cmd.map ApiRequest
                        , Api.Routes.Main.buildRequest
                            (Api.Routes.Main.buildPayload
                                (UpdateFilterableAttrs uid
                                    (List.filter (\x -> x.enabled == True) model.pages.attributes.filterable
                                        |> List.map (\x -> x.title)
                                    )
                                    Api.Routes.Main.settingsUpdateDecoder
                                )
                                rootUrl
                            )
                            (getSavedToken model)
                            |> Cmd.map ApiRequest
                        , Api.Routes.Main.buildRequest
                            (Api.Routes.Main.buildPayload
                                (UpdateSearchableAttrs uid
                                    (List.filter (\x -> x.enabled == True) model.pages.attributes.searchable
                                        |> List.map (\x -> x.title)
                                    )
                                    Api.Routes.Main.settingsUpdateDecoder
                                )
                                rootUrl
                            )
                            (getSavedToken model)
                            |> Cmd.map ApiRequest
                        , Api.Routes.Main.buildRequest
                            (Api.Routes.Main.buildPayload
                                (UpdateSortableAttrs uid
                                    (List.filter (\x -> x.enabled == True) model.pages.attributes.sortable
                                        |> List.map (\x -> x.title)
                                    )
                                    Api.Routes.Main.settingsUpdateDecoder
                                )
                                rootUrl
                            )
                            (getSavedToken model)
                            |> Cmd.map ApiRequest
                        , case AttributesPage.getDistinctAttr model.pages.attributes.distinct of
                            Just da ->
                                Api.Routes.Main.buildRequest
                                    (Api.Routes.Main.buildPayload
                                        (UpdateDistinctAttr uid
                                            da
                                            Api.Routes.Main.settingsUpdateDecoder
                                        )
                                        rootUrl
                                    )
                                    (getSavedToken model)
                                    |> Cmd.map ApiRequest

                            Nothing ->
                                Cmd.none
                        ]
                    )

                Nothing ->
                    ( model, Cmd.none )

        AttributesPage.Reset ->
            update (SidebarMsg (SelectPage model.pages.selectedPage)) model

        _ ->
            let
                updatedAttrsViewModel =
                    AttributesPage.update
                        msg
                        model.pages.attributes
            in
            ( { model
                | pages = updateAttributesViewModel model.pages updatedAttrsViewModel
              }
            , Cmd.none
            )


handleSynonymsViewMsg : Model -> SynonymsPage.Msg -> ( Model, Cmd Msg )
handleSynonymsViewMsg model msg =
    case msg of
        m ->
            case m of
                SynonymsPage.Sync ->
                    let
                        ( updatedSynonymsViewModel, _ ) =
                            SynonymsPage.update msg (getSynonymsViewModel model)

                        currentSynonyms =
                            List.map
                                (\s -> ( s.synonymKey, s.synonymList ))
                                model.pages.synonyms.synonymCards
                                |> List.filter (\( k, v ) -> k /= "" && v /= [])

                        updatedModel =
                            { model
                                | pages = updateSynonymsViewModel model.pages updatedSynonymsViewModel
                            }
                    in
                    case getCurrentlySelectedIndexId updatedModel of
                        Just indexUid ->
                            ( updatedModel
                            , Api.Routes.Main.buildRequest
                                (Api.Routes.Main.buildPayload
                                    (UpdateSynonyms indexUid
                                        (Dict.fromList currentSynonyms)
                                        Api.Routes.Main.settingsUpdateDecoder
                                    )
                                    (getRootUrl model)
                                )
                                (getSavedToken model)
                                |> Cmd.map ApiRequest
                            )

                        Nothing ->
                            ( updatedModel, Cmd.none )

                SynonymsPage.Reset ->
                    update (SidebarMsg (SelectPage model.pages.selectedPage)) model

                _ ->
                    let
                        ( updatedSynonymsViewModel, _ ) =
                            SynonymsPage.update msg (getSynonymsViewModel model)
                    in
                    ( { model
                        | pages = updateSynonymsViewModel model.pages updatedSynonymsViewModel
                      }
                    , Cmd.none
                    )


handleStopWordsViewMsg : Model -> StopWordsPage.Msg -> ( Model, Cmd Msg )
handleStopWordsViewMsg model msg =
    case msg of
        StopWordsPage.Sync ->
            case getCurrentlySelectedIndexId model of
                Just uid ->
                    ( model
                    , Api.Routes.Main.buildRequest
                        (Api.Routes.Main.buildPayload
                            (UpdateStopWords uid
                                (List.map (\x -> x.title) model.pages.stopWords.words)
                                Api.Routes.Main.settingsUpdateDecoder
                            )
                            (getRootUrl model)
                        )
                        (getSavedToken model)
                        |> Cmd.map ApiRequest
                    )

                Nothing ->
                    ( model, Cmd.none )

        StopWordsPage.Reset ->
            let
                ( m, _ ) =
                    StopWordsPage.update msg model.pages.stopWords

                um =
                    { model | pages = updateStopWordsViewModel model.pages m }
            in
            update (SidebarMsg (SelectPage model.pages.selectedPage)) um

        _ ->
            let
                ( m, _ ) =
                    StopWordsPage.update msg model.pages.stopWords
            in
            ( { model | pages = updateStopWordsViewModel model.pages m }, Cmd.none )


handleSettingsViewMsg : Model -> SettingsPage.Msg -> ( Model, Cmd Msg )
handleSettingsViewMsg model msg =
    let
        ( updatedSettingsPageViewModel, _ ) =
            SettingsPage.update msg model.pages.settings

        pagesModel =
            model.pages

        updatedPagesModel =
            { pagesModel
                | settings = updatedSettingsPageViewModel
                , selectedPage = Settings updatedSettingsPageViewModel
            }

        updatedModelValue =
            { model
                | pages = updatedPagesModel
            }
    in
    case msg of
        SettingsPage.Save ->
            ( updatedModelValue
            , Api.Routes.Main.buildRequest
                (Api.Routes.Main.buildPayload (ListIndexes Api.Routes.Main.indexesRouteResponseListDecoder) updatedSettingsPageViewModel.endpointValue)
                (getSavedToken model)
                |> Cmd.map ApiRequest
            )

        _ ->
            ( updatedModelValue, Cmd.none )


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
                    case getCurrentlySelectedIndexId model of
                        Just uid ->
                            ( updatedModel
                            , Api.Routes.Main.buildRequest
                                (Api.Routes.Main.buildPayload (ListDocuments uid) (getRootUrl model))
                                (getSavedToken model)
                                |> Cmd.map ApiRequest
                            )

                        Nothing ->
                            let
                                updatedDocsPage =
                                    DocumentsPage.update DocumentsPage.Clear model.pages.documents

                                up =
                                    updateDocumentsViewModel model.pages updatedDocsPage
                            in
                            ( { updatedModel
                                | pages = up
                              }
                            , Cmd.none
                            )

                Synonyms m ->
                    case getCurrentlySelectedIndexId updatedModel of
                        Just indexUid ->
                            ( updatedModel
                            , Api.Routes.Main.buildRequest
                                (Api.Routes.Main.buildPayload (ListSynonyms indexUid Api.Routes.Main.synonymsListDecoder) (getRootUrl model))
                                (getSavedToken model)
                                |> Cmd.map ApiRequest
                            )

                        Nothing ->
                            ( updatedModel, Cmd.none )

                StopWords _ ->
                    case getCurrentlySelectedIndexId updatedModel of
                        Just indexUid ->
                            ( updatedModel
                            , Api.Routes.Main.buildRequest
                                (Api.Routes.Main.buildPayload (ListStopWords indexUid Api.Routes.Main.maybeStringListDecoder) (getRootUrl model))
                                (getSavedToken model)
                                |> Cmd.map ApiRequest
                            )

                        Nothing ->
                            ( updatedModel, Cmd.none )

                Attributes _ ->
                    case getCurrentlySelectedIndexId updatedModel of
                        Just indexUid ->
                            ( updatedModel
                            , Api.Routes.Main.buildRequest
                                (Api.Routes.Main.buildPayload (Stats indexUid statsDecoder) (getRootUrl model))
                                (getSavedToken model)
                                |> Cmd.map ApiRequest
                            )

                        Nothing ->
                            ( updatedModel, Cmd.none )

        Sidebar.DropdownMsg d ->
            case d of
                Select i ->
                    -- update UI
                    let
                        ( updatedSidebarModel, _ ) =
                            Sidebar.update sidebarMsg model.sidebarModel

                        updatedModel =
                            { model | sidebarModel = updatedSidebarModel }
                    in
                    handleSidebarSelection updatedModel (Sidebar.SelectPage updatedModel.pages.selectedPage)

                _ ->
                    let
                        ( updatedSidebarModel, _ ) =
                            Sidebar.update sidebarMsg model.sidebarModel
                    in
                    ( { model | sidebarModel = updatedSidebarModel }, Cmd.none )

        ClearDropdownOptions ->
            ( purgeModel model, Cmd.none )


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
                SweetPoll.init (taskConfigBuilder taskId (getRootUrl model))
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
    , dropDown = model.sidebarModel.dropDown
    }


getSynonymsViewModel : Model -> SynonymsPage.Model
getSynonymsViewModel model =
    model.pages.synonyms



-- VIEW MODEL SETTERS


purgeModel : Model -> Model
purgeModel model =
    let
        ( s, _ ) =
            Sidebar.update Sidebar.ClearDropdownOptions model.sidebarModel

        documents =
            DocumentsPage.update DocumentsPage.Clear model.pages.documents

        ( synonyms, _ ) =
            SynonymsPage.update SynonymsPage.Purge model.pages.synonyms

        ( stopWords, _ ) =
            StopWordsPage.update StopWordsPage.Purge model.pages.stopWords

        attributes =
            AttributesPage.update AttributesPage.Purge model.pages.attributes

        selectedPage =
            model.pages.selectedPage

        settings =
            model.pages.settings
    in
    { model
        | sidebarModel = s
        , pages =
            { documents = documents
            , settings = settings
            , synonyms = synonyms
            , stopWords = stopWords
            , attributes = attributes
            , selectedPage = selectedPage
            }
    }


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
            taskConfigBuilder taskId (getRootUrl model)

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
                                            SynonymsPage.updateSyncStatusState model.pages.synonyms.synonymCards Fired

                                        updatedSynonymsPageViewModel =
                                            { synonymCards = updatedSynonyms, deletionQueue = model.pages.synonyms.deletionQueue }
                                    in
                                    ( { model
                                        | pollingQueue =
                                            List.map
                                                (updatePollState task newState)
                                                model.pollingQueue
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
                                            SynonymsPage.updateSyncStatusState model.pages.synonyms.synonymCards Success

                                        updatedSynonymsPageViewModel =
                                            { synonymCards = updatedSynonyms, deletionQueue = [] }
                                    in
                                    ( { model
                                        | pollingQueue = List.filter (\( x, _ ) -> x /= task) model.pollingQueue
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
                                        , pages = updateStopWordsViewModel model.pages updatedStopWordsViewModel
                                      }
                                    , Cmd.none
                                    )

                                UpdateAttributeTask _ _ attrType ->
                                    let
                                        attrsPageViewModel =
                                            AttributesPage.update
                                                (AttributesPage.UpdateSyncStatusState attrType Success)
                                                model.pages.attributes
                                    in
                                    ( { model
                                        | pollingQueue = List.filter (\( x, _ ) -> x /= task) model.pollingQueue
                                        , pages = updateAttributesViewModel model.pages attrsPageViewModel
                                      }
                                    , Cmd.none
                                    )

                        "failed" ->
                            case task of
                                UpdateSynonymsTask _ _ ->
                                    let
                                        updatedSynonyms =
                                            SynonymsPage.updateSyncStatusState model.pages.synonyms.synonymCards Failed

                                        updatedSynonymsPageViewModel =
                                            { synonymCards = updatedSynonyms, deletionQueue = model.pages.synonyms.deletionQueue }
                                    in
                                    ( { model
                                        | pollingQueue = List.filter (\( x, _ ) -> x /= task) model.pollingQueue
                                        , pages = updateSynonymsViewModel model.pages updatedSynonymsPageViewModel
                                      }
                                    , Cmd.none
                                    )

                                UpdateAttributeTask _ _ t ->
                                    let
                                        attrsPageViewModel =
                                            AttributesPage.update
                                                (AttributesPage.UpdateSyncStatusState t Failed)
                                                model.pages.attributes
                                    in
                                    ( { model
                                        | pages = updateAttributesViewModel model.pages attrsPageViewModel
                                        , pollingQueue = List.filter (\( x, _ ) -> x /= task) model.pollingQueue
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


buildSynonymsViewModelFromApiResponse : Dict String (List String) -> List UI.Components.SynonymCard.Model
buildSynonymsViewModelFromApiResponse d =
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
                , saved = Just ( title, values )
                }
            )


getCurrentlySelectedIndexId : Model -> Maybe String
getCurrentlySelectedIndexId model =
    case model.sidebarModel.dropDown.selectedValue of
        Just i ->
            Just i.title

        Nothing ->
            Nothing


getSavedToken : Model -> String
getSavedToken model =
    model.pages.settings.savedTokenValue


getRootUrl : Model -> String
getRootUrl model =
    model.pages.settings.savedEndpointValue


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            { pages = Views.init
            , pollingQueue = []
            , sidebarModel = Sidebar.init
            }
    in
    ( model
    , Api.Routes.Main.buildRequest
        (Api.Routes.Main.buildPayload (ListIndexes Api.Routes.Main.indexesRouteResponseListDecoder) (getRootUrl model))
        (getSavedToken model)
        |> Cmd.map ApiRequest
    )
