module Api exposing (..)

-- Endpoints


type Request
    = Indexes


type Indexes
    = List
    | Show String
    | Create ( String, Maybe String )
    | Update String
    | Delete String



-- Decoders
