module UI.Views exposing (..)

import Element exposing (..)


type Page
    = Indexes
    | Settings
    | Search
    | Stats
    | Documents
    | Keys
    | Tasks


pageTitle : Page -> String
pageTitle page =
    case page of
        Indexes ->
            "Indexes"

        Settings ->
            "Settings"

        Search ->
            "Search"

        Stats ->
            "Stats"

        Documents ->
            "Documents"

        Keys ->
            "Keys"

        Tasks ->
            "Tasks"
