module Api.Routes.Main exposing (..)

import Api.Helper exposing (RequestMethod(..), getRequestMethodTitle, headers, rootUrl)
import Dict exposing (Dict)
import Http
import Json.Decode exposing (Decoder, bool, decodeString, field, int, keyValuePairs, list, string)
import Json.Encode as Encode
import SweetPoll


type Msg
    = HandleListResponse (Result Http.Error (List IndexesRouteResponseListItem))
    | HandleShowResponse (Result Http.Error IndexesRouteResponseListItem)
    | HandleDocumentsResponse (Result Http.Error String)
    | HandleListStopWordsResponse (Result Http.Error (List String)) String
    | HandleUpdateSynonymsResponse (Result Http.Error SettingsRouteResponseItem)
    | HandleListSynonymsResponse (Result Http.Error (Dict String (List String))) String
    | HandleIndexKeysResponse IndexKeys
    | HandleListDisplayedAttrsResponse (Result Http.Error (List String)) String
    | HandleListSearchableAttrsResponse (Result Http.Error (List String)) String
    | HandleListSortableAttrsResponse (Result Http.Error (List String)) String
    | HandleListFilterableAttrsResponse (Result Http.Error (List String)) String
    | HandleListDistinctAttrResponse (Result Http.Error (Maybe String)) String
    | HandleUpdateDisplayedAttrsResponse (Result Http.Error SettingsRouteResponseItem)
    | HandleUpdateFilterableAttrsResponse (Result Http.Error SettingsRouteResponseItem)
    | HandleUpdateSortableAttrsResponse (Result Http.Error SettingsRouteResponseItem)
    | HandleUpdateSearchableAttrsResponse (Result Http.Error SettingsRouteResponseItem)
    | HandleUpdateDistinctAttrResponse (Result Http.Error SettingsRouteResponseItem)
    | HandleUpdateStopWordsResponse (Result Http.Error SettingsRouteResponseItem)
    | HandleStatsResponse (Result Http.Error IndexStats) String


type Route
    = ListIndexes (Decoder (List IndexesRouteResponseListItem))
    | ShowIndex String (Decoder IndexesRouteResponseListItem)
    | CreateIndex ( String, Maybe String )
    | UpdateIndex String
    | DeleteIndex String
    | ListDocuments String
    | ListStopWords String (Decoder (List String))
    | ResetStopWords String
    | GetTask Int
    | UpdateSynonyms String (Dict String (List String)) (Decoder SettingsRouteResponseItem)
    | ListSynonyms String (Decoder (Dict String (List String)))
    | ListDisplayedAttrs String (Decoder (List String))
    | ListSearchableAttrs String (Decoder (List String))
    | ListSortableAttrs String (Decoder (List String))
    | ListFilterableAttrs String (Decoder (List String))
    | ListDistinctAttr String (Decoder (Maybe String))
    | UpdateDisplayedAttrs String (List String) (Decoder SettingsRouteResponseItem)
    | UpdateSearchableAttrs String (List String) (Decoder SettingsRouteResponseItem)
    | UpdateSortableAttrs String (List String) (Decoder SettingsRouteResponseItem)
    | UpdateFilterableAttrs String (List String) (Decoder SettingsRouteResponseItem)
    | UpdateDistinctAttr String String (Decoder SettingsRouteResponseItem)
    | UpdateStopWords String (List String) (Decoder SettingsRouteResponseItem)
    | Stats String (Decoder IndexStats)


type alias Payload =
    { method : RequestMethod
    , endpoint : String
    , body : Http.Body
    , route : Route
    }


type alias IndexesRouteResponseListItem =
    { uid : String
    , name : String
    , createdAt : String
    , updatedAt : String
    , primaryKey : String
    }


type alias SettingsRouteResponseItem =
    { uid : Int
    , indexUid : String
    }


type alias IndexKeys =
    { indexUid : String
    , keys : List String
    }


type alias IndexStats =
    { numberOfDocuments : Int
    , isIndexing : Bool
    , fieldDistribution : Dict String Int
    }



-- Builders


buildRequest : Payload -> String -> Cmd Msg
buildRequest payload token =
    let
        r =
            payload.route
    in
    case r of
        ListIndexes d ->
            Http.request
                { method = getRequestMethodTitle payload.method
                , headers = headers token
                , url = payload.endpoint
                , body = payload.body
                , expect = Http.expectJson HandleListResponse d
                , timeout = Nothing
                , tracker = Nothing
                }

        ShowIndex _ d ->
            Http.request
                { method = getRequestMethodTitle payload.method
                , headers = headers token
                , url = payload.endpoint
                , body = payload.body
                , expect = Http.expectJson HandleShowResponse d
                , timeout = Nothing
                , tracker = Nothing
                }

        CreateIndex _ ->
            Debug.todo "branch 'Create _' not implemented"

        UpdateIndex _ ->
            Debug.todo "branch 'Update _' not implemented"

        DeleteIndex _ ->
            Debug.todo "branch 'Delete _' not implemented"

        ListDocuments _ ->
            Http.request
                { method = getRequestMethodTitle payload.method
                , headers = headers token
                , url = payload.endpoint
                , body = payload.body
                , expect = Http.expectString HandleDocumentsResponse
                , timeout = Nothing
                , tracker = Nothing
                }

        ListStopWords x d ->
            Http.request
                { method = getRequestMethodTitle payload.method
                , headers = headers token
                , url = payload.endpoint
                , body = payload.body
                , expect = Http.expectJson HandleListStopWordsResponse d
                , timeout = Nothing
                , tracker = Nothing
                }
                |> Cmd.map (\a -> a x)

        UpdateStopWords _ _ d ->
            Http.request
                { method = getRequestMethodTitle payload.method
                , headers = headers token
                , url = payload.endpoint
                , body = payload.body
                , expect = Http.expectJson HandleUpdateStopWordsResponse d
                , timeout = Nothing
                , tracker = Nothing
                }

        ResetStopWords _ ->
            Debug.todo "branch 'ResetStopWords' not implemented"

        GetTask _ ->
            Debug.todo "branch 'Get Task' not implemented"

        UpdateSynonyms _ _ d ->
            Http.request
                { method = getRequestMethodTitle payload.method
                , headers = headers token
                , url = payload.endpoint
                , body = payload.body
                , expect = Http.expectJson HandleUpdateSynonymsResponse d
                , timeout = Nothing
                , tracker = Nothing
                }

        ListSynonyms x d ->
            Http.request
                { method = getRequestMethodTitle payload.method
                , headers = headers token
                , url = payload.endpoint
                , body = payload.body
                , expect = Http.expectJson HandleListSynonymsResponse d
                , timeout = Nothing
                , tracker = Nothing
                }
                |> Cmd.map (\a -> a x)

        ListDisplayedAttrs x d ->
            Http.request
                { method = getRequestMethodTitle payload.method
                , headers = headers token
                , url = payload.endpoint
                , body = payload.body
                , expect = Http.expectJson HandleListDisplayedAttrsResponse d
                , timeout = Nothing
                , tracker = Nothing
                }
                |> Cmd.map (\a -> a x)

        ListSearchableAttrs x d ->
            Http.request
                { method = getRequestMethodTitle payload.method
                , headers = headers token
                , url = payload.endpoint
                , body = payload.body
                , expect = Http.expectJson HandleListSearchableAttrsResponse d
                , timeout = Nothing
                , tracker = Nothing
                }
                |> Cmd.map (\a -> a x)

        ListFilterableAttrs x d ->
            Http.request
                { method = getRequestMethodTitle payload.method
                , headers = headers token
                , url = payload.endpoint
                , body = payload.body
                , expect = Http.expectJson HandleListFilterableAttrsResponse d
                , timeout = Nothing
                , tracker = Nothing
                }
                |> Cmd.map (\a -> a x)

        ListSortableAttrs x d ->
            Http.request
                { method = getRequestMethodTitle payload.method
                , headers = headers token
                , url = payload.endpoint
                , body = payload.body
                , expect = Http.expectJson HandleListSortableAttrsResponse d
                , timeout = Nothing
                , tracker = Nothing
                }
                |> Cmd.map (\a -> a x)

        ListDistinctAttr x d ->
            Http.request
                { method = getRequestMethodTitle payload.method
                , headers = headers token
                , url = payload.endpoint
                , body = payload.body
                , expect = Http.expectJson HandleListDistinctAttrResponse d
                , timeout = Nothing
                , tracker = Nothing
                }
                |> Cmd.map (\a -> a x)

        UpdateDisplayedAttrs _ _ d ->
            Http.request
                { method = getRequestMethodTitle payload.method
                , headers = headers token
                , url = payload.endpoint
                , body = payload.body
                , expect = Http.expectJson HandleUpdateDisplayedAttrsResponse d
                , timeout = Nothing
                , tracker = Nothing
                }

        UpdateFilterableAttrs _ _ d ->
            Http.request
                { method = getRequestMethodTitle payload.method
                , headers = headers token
                , url = payload.endpoint
                , body = payload.body
                , expect = Http.expectJson HandleUpdateFilterableAttrsResponse d
                , timeout = Nothing
                , tracker = Nothing
                }

        UpdateSearchableAttrs _ _ d ->
            Http.request
                { method = getRequestMethodTitle payload.method
                , headers = headers token
                , url = payload.endpoint
                , body = payload.body
                , expect = Http.expectJson HandleUpdateSearchableAttrsResponse d
                , timeout = Nothing
                , tracker = Nothing
                }

        UpdateSortableAttrs _ _ d ->
            Http.request
                { method = getRequestMethodTitle payload.method
                , headers = headers token
                , url = payload.endpoint
                , body = payload.body
                , expect = Http.expectJson HandleUpdateSortableAttrsResponse d
                , timeout = Nothing
                , tracker = Nothing
                }

        UpdateDistinctAttr _ _ d ->
            Http.request
                { method = getRequestMethodTitle payload.method
                , headers = headers token
                , url = payload.endpoint
                , body = payload.body
                , expect = Http.expectJson HandleUpdateDistinctAttrResponse d
                , timeout = Nothing
                , tracker = Nothing
                }

        Stats x d ->
            Http.request
                { method = getRequestMethodTitle payload.method
                , headers = headers token
                , url = payload.endpoint
                , body = payload.body
                , expect = Http.expectJson HandleStatsResponse d
                , timeout = Nothing
                , tracker = Nothing
                }
                |> Cmd.map (\a -> a x)


buildPayload : Route -> Payload
buildPayload r =
    case r of
        ListIndexes _ ->
            { method = GET, endpoint = rootUrl ++ "/indexes", body = Http.emptyBody, route = r }

        ShowIndex i _ ->
            { method = GET, endpoint = rootUrl ++ "/indexes/" ++ i, body = Http.emptyBody, route = r }

        CreateIndex ( i, k ) ->
            case k of
                Just key ->
                    let
                        body =
                            Encode.object
                                [ ( "uid", Encode.string i )
                                , ( "primaryKey", Encode.string key )
                                ]
                    in
                    { method = POST, endpoint = rootUrl ++ "/indexes", body = Http.jsonBody body, route = r }

                Nothing ->
                    let
                        body =
                            Encode.object
                                [ ( "uid", Encode.string i )
                                ]
                    in
                    { method = POST, endpoint = rootUrl ++ "/indexes", body = Http.jsonBody body, route = r }

        UpdateIndex i ->
            { method = PUT, endpoint = rootUrl ++ "/indexes/" ++ i, body = Http.emptyBody, route = r }

        DeleteIndex i ->
            { method = DELETE, endpoint = rootUrl ++ "/indexes/" ++ i, body = Http.emptyBody, route = r }

        ListDocuments i ->
            { method = GET, endpoint = rootUrl ++ "/indexes/" ++ i ++ "/documents", body = Http.emptyBody, route = r }

        ListStopWords i _ ->
            { method = GET, endpoint = rootUrl ++ "/indexes/" ++ i ++ "/settings/stop-words", body = Http.emptyBody, route = r }

        UpdateStopWords i w _ ->
            let
                body =
                    Encode.list Encode.string w
            in
            { method = POST, endpoint = rootUrl ++ "/indexes/" ++ i ++ "/settings/stop-words", body = Http.jsonBody body, route = r }

        ResetStopWords i ->
            { method = DELETE, endpoint = rootUrl ++ "/indexes/" ++ i ++ "/settings/stop-words", body = Http.emptyBody, route = r }

        GetTask i ->
            { method = GET, endpoint = rootUrl ++ "/tasks/" ++ String.fromInt i, body = Http.emptyBody, route = r }

        UpdateSynonyms i dic _ ->
            let
                body =
                    Encode.dict identity (Encode.list Encode.string) dic
            in
            { method = POST, endpoint = rootUrl ++ "/indexes/" ++ i ++ "/settings/synonyms", body = Http.jsonBody body, route = r }

        ListSynonyms i _ ->
            { method = GET, endpoint = rootUrl ++ "/indexes/" ++ i ++ "/settings/synonyms", body = Http.emptyBody, route = r }

        ListDisplayedAttrs i _ ->
            { method = GET, endpoint = rootUrl ++ "/indexes/" ++ i ++ "/settings/displayed-attributes", body = Http.emptyBody, route = r }

        ListSearchableAttrs i _ ->
            { method = GET, endpoint = rootUrl ++ "/indexes/" ++ i ++ "/settings/searchable-attributes", body = Http.emptyBody, route = r }

        ListSortableAttrs i _ ->
            { method = GET, endpoint = rootUrl ++ "/indexes/" ++ i ++ "/settings/sortable-attributes", body = Http.emptyBody, route = r }

        ListFilterableAttrs i _ ->
            { method = GET, endpoint = rootUrl ++ "/indexes/" ++ i ++ "/settings/filterable-attributes", body = Http.emptyBody, route = r }

        ListDistinctAttr i _ ->
            { method = GET, endpoint = rootUrl ++ "/indexes/" ++ i ++ "/settings/distinct-attribute", body = Http.emptyBody, route = r }

        UpdateDisplayedAttrs i attrs _ ->
            let
                body =
                    Encode.list Encode.string attrs
            in
            { method = POST, endpoint = rootUrl ++ "/indexes/" ++ i ++ "/settings/displayed-attributes", body = Http.jsonBody body, route = r }

        UpdateFilterableAttrs i attrs _ ->
            let
                body =
                    Encode.list Encode.string attrs
            in
            { method = POST, endpoint = rootUrl ++ "/indexes/" ++ i ++ "/settings/filterable-attributes", body = Http.jsonBody body, route = r }

        UpdateSortableAttrs i attrs _ ->
            let
                body =
                    Encode.list Encode.string attrs
            in
            { method = POST, endpoint = rootUrl ++ "/indexes/" ++ i ++ "/settings/sortable-attributes", body = Http.jsonBody body, route = r }

        UpdateSearchableAttrs i attrs _ ->
            let
                body =
                    Encode.list Encode.string attrs
            in
            { method = POST, endpoint = rootUrl ++ "/indexes/" ++ i ++ "/settings/searchable-attributes", body = Http.jsonBody body, route = r }

        UpdateDistinctAttr i attrs _ ->
            let
                body =
                    Encode.string attrs
            in
            { method = POST, endpoint = rootUrl ++ "/indexes/" ++ i ++ "/settings/distinct-attribute", body = Http.jsonBody body, route = r }

        Stats i _ ->
            { method = GET, endpoint = rootUrl ++ "/indexes/" ++ i ++ "/stats", body = Http.emptyBody, route = r }



-- Decoders


synonymsListDecoder : Decoder (Dict String (List String))
synonymsListDecoder =
    Json.Decode.dict (list string)


indexesRouteResponseListDecoder : Decoder (List IndexesRouteResponseListItem)
indexesRouteResponseListDecoder =
    Json.Decode.list indexesRouteResponseListItemDecoder


indexesRouteResponseListItemDecoder : Decoder IndexesRouteResponseListItem
indexesRouteResponseListItemDecoder =
    Json.Decode.map5 IndexesRouteResponseListItem
        (field
            "uid"
            string
        )
        (field
            "name"
            string
        )
        (field
            "createdAt"
            string
        )
        (field
            "updatedAt"
            string
        )
        (field
            "primaryKey"
            string
        )


maybeStringListDecoder : Decoder (List String)
maybeStringListDecoder =
    Json.Decode.list string


stringDecoder : Decoder (Maybe String)
stringDecoder =
    Json.Decode.maybe string


taskConfigBuilder : Int -> SweetPoll.Config String
taskConfigBuilder id =
    let
        payload =
            buildPayload (GetTask id)
    in
    { url = payload.endpoint
    , decoder = field "status" string
    , delay = 10
    , samesBeforeDelay = 8
    , delayMultiplier = 2
    , maxDelay = 1000
    }


settingsUpdateDecoder : Decoder SettingsRouteResponseItem
settingsUpdateDecoder =
    Json.Decode.map2 SettingsRouteResponseItem
        (field
            "uid"
            int
        )
        (field
            "indexUid"
            string
        )


indexKeysDecoder : Decoder IndexKeys
indexKeysDecoder =
    Json.Decode.map2 IndexKeys
        (field
            "indexUid"
            string
        )
        (field
            "keys"
            (Json.Decode.list string)
        )


attrsDecoder : String -> Result Json.Decode.Error (List ( String, String ))
attrsDecoder val =
    decodeString (keyValuePairs string) val


statsDecoder : Decoder IndexStats
statsDecoder =
    Json.Decode.map3 IndexStats
        (field
            "numberOfDocuments"
            int
        )
        (field
            "isIndexing"
            bool
        )
        (field
            "fieldDistribution"
            (Json.Decode.dict int)
        )
