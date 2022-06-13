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
