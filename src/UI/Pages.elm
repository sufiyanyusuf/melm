module UI.Pages exposing (..)

import Element exposing (..)
import UI.PageViews.Documents as Documents
import UI.PageViews.Indexes as Indexes
import UI.PageViews.Settings as Settings


type Page
    = Indexes Indexes.Model
    | Settings Settings.Model
    | Search
    | Stats
    | Documents Documents.Model
    | Keys
    | Tasks


init : List Page
init =
    [ Indexes Indexes.init
    , Settings Settings.init
    , Search
    , Stats
    , Documents Documents.init
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
