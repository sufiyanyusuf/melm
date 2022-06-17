module Request exposing (..)


type Request
    = Create
    | Update
    | Delete


type RequestStatus
    = NoRequest
    | Fired
    | Success
    | Failed


getRequestStatus : RequestStatus -> String
getRequestStatus r =
    case r of
        NoRequest ->
            ""

        Fired ->
            "Loading"

        Success ->
            "Success"

        Failed ->
            "Failed"
