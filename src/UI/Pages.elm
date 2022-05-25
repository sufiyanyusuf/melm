module UI.Pages exposing (..)

import Element exposing (..)
import UI.Pages.Settings as Settings


type Page
    = Indexes
    | Settings Settings.Model
    | Search
    | Stats
    | Documents
    | Keys
    | Tasks


init : List Page
init =
    [ Indexes
    , Settings Settings.init
    , Search
    , Stats
    , Documents
    , Keys
    , Tasks
    ]



-- pageTitle : Page -> String
-- pageTitle page =
--     case page of
--         Indexes ->
--             "Indexes"
--         Settings _ ->
--             "Settings"
--         Search ->
--             "Search"
--         Stats ->
--             "Stats"
--         Documents ->
--             "Documents"
--         Keys ->
--             "Keys"
--         Tasks ->
--             "Tasks"
