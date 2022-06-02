module Api.Routes.Main exposing (..)

import Api.Helper exposing (RequestMethod(..), getRequestMethodTitle, headers, rootUrl)
import Dict exposing (Dict)
import Http
import Json.Decode exposing (Decoder, bool, dict, field, float, int, list, map2, maybe, string)
import Json.Encode as Encode


type DictValue
    = String String
    | Int Int


type Msg
    = HandleListResponse (Result Http.Error (List IndexesRouteResponseListItem))
    | HandleShowResponse (Result Http.Error IndexesRouteResponseListItem)
    | HandleDocumentsResponse (Result Http.Error String)
    | HandleListStopWordsResponse (Result Http.Error (List String))


type Route
    = ListIndexes (Decoder (List IndexesRouteResponseListItem))
    | ShowIndex String (Decoder IndexesRouteResponseListItem)
    | CreateIndex ( String, Maybe String )
    | UpdateIndex String
    | DeleteIndex String
    | ListDocuments String
    | ListStopWords String (Decoder (List String))
    | UpdateStopWords String (List String)
    | ResetStopWords String


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

        ListStopWords _ d ->
            Http.request
                { method = getRequestMethodTitle payload.method
                , headers = headers token
                , url = payload.endpoint
                , body = payload.body
                , expect = Http.expectJson HandleListStopWordsResponse d
                , timeout = Nothing
                , tracker = Nothing
                }

        UpdateStopWords _ _ ->
            Debug.todo "branch 'UpdateStopWords _' not implemented"

        ResetStopWords _ ->
            Debug.todo "branch 'ResetStopWords' not implemented"


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

        UpdateStopWords _ _ ->
            Debug.todo "branch 'UpdateStopWords _' not implemented"

        ResetStopWords i ->
            { method = DELETE, endpoint = rootUrl ++ "/indexes/" ++ i ++ "/settings/stop-words", body = Http.emptyBody, route = r }



-- Decoders


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


stopWordsListItemDecoder : Decoder (List String)
stopWordsListItemDecoder =
    Json.Decode.list string
