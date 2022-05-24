module Api.Routes.Main exposing (..)

import Api.Helper exposing (RequestMethod(..), getRequestMethodTitle, headers, rootUrl)
import Http
import Json.Decode exposing (Decoder, bool, field, float, int, list, map2, maybe, string)
import Json.Encode as Encode


type Msg
    = HandleListResponse (Result Http.Error (List IndexesRouteResponseListItem))
    | HandleShowResponse (Result Http.Error IndexesRouteResponseListItem)


type Route
    = List (Decoder (List IndexesRouteResponseListItem))
    | Show String (Decoder IndexesRouteResponseListItem)
    | Create ( String, Maybe String )
    | Update String
    | Delete String


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


buildEffect : Payload -> String -> Cmd Msg
buildEffect p t =
    let
        r =
            p.route
    in
    case r of
        List d ->
            Http.request
                { method = getRequestMethodTitle p.method
                , headers = headers t
                , url = p.endpoint
                , body = p.body
                , expect = Http.expectJson HandleListResponse d
                , timeout = Just 20.0
                , tracker = Nothing
                }

        Show _ d ->
            Http.request
                { method = getRequestMethodTitle p.method
                , headers = headers t
                , url = p.endpoint
                , body = p.body
                , expect = Http.expectJson HandleShowResponse d
                , timeout = Just 20.0
                , tracker = Nothing
                }

        Create _ ->
            Debug.todo "branch 'Create _' not implemented"

        Update _ ->
            Debug.todo "branch 'Update _' not implemented"

        Delete _ ->
            Debug.todo "branch 'Delete _' not implemented"


buildPayload : Route -> Payload
buildPayload r =
    case r of
        List _ ->
            { method = GET, endpoint = rootUrl ++ "/indexes", body = Http.emptyBody, route = r }

        Show i _ ->
            { method = GET, endpoint = rootUrl ++ "/indexes/" ++ i, body = Http.emptyBody, route = r }

        Create ( i, k ) ->
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

        Update i ->
            { method = PUT, endpoint = rootUrl ++ "/indexes/" ++ i, body = Http.emptyBody, route = r }

        Delete i ->
            { method = DELETE, endpoint = rootUrl ++ "/indexes/" ++ i, body = Http.emptyBody, route = r }



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
