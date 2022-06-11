module UI.Pages exposing (..)

import Element exposing (..)
import UI.PageViews.Attributes as Attributes
import UI.PageViews.Documents as Documents
import UI.PageViews.Settings as Settings
import UI.PageViews.StopWords as StopWords
import UI.PageViews.Synonyms as Synonyms


type Page
    = Settings Settings.Model
    | Stats
    | Documents Documents.Model
    | Tasks
    | RankingRules
    | Synonyms Synonyms.Model
    | StopWords StopWords.Model
    | Attributes Attributes.Model


init : String -> List Page
init indexUid =
    [ Documents Documents.init
    , Tasks
    , RankingRules
    , Synonyms (Synonyms.init indexUid)
    , StopWords StopWords.init
    , Stats
    , Settings Settings.init
    , Attributes Attributes.init
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
