module Api.Helper exposing (..)

import Http



-- Basic HTTP Stuff


type RequestMethod
    = GET
    | POST
    | UPDATE
    | DELETE
    | PUT


getRequestMethodTitle : RequestMethod -> String
getRequestMethodTitle requestMethod =
    case requestMethod of
        GET ->
            "GET"

        POST ->
            "POST"

        UPDATE ->
            "UPDATE"

        DELETE ->
            "DELETE"

        PUT ->
            "PUT"


headers : String -> List Http.Header
headers token =
    [ Http.header "Authorization" ("Bearer " ++ token) ]
