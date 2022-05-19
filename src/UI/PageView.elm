module UI.PageView exposing (..)

import Element exposing (..)
import UI.Pages.Documents as DocumentsView
import UI.Pages.Indexes as IndexesView
import UI.Pages.Keys as KeysView
import UI.Pages.Search as SearchView
import UI.Pages.Settings as SettingsView
import UI.Pages.Stats as StatsView
import UI.Pages.Tasks as TasksView
import UI.Views as Views exposing (Page)



-- Msg


type Msg
    = IndexesViewMsg IndexesView.Msg
    | SettingsViewMsg SettingsView.Msg
    | SearchViewMsg SearchView.Msg
    | StatsViewMsg StatsView.Msg
    | DocumentsViewMsg DocumentsView.Msg
    | KeysViewMsg KeysView.Msg
    | TasksViewMsg TasksView.Msg



-- View


view : Page -> Element Msg
view currentPage =
    el
        [ width fill
        , height fill
        , padding 12
        , scrollbarY
        ]
        (getCurrentPageView currentPage)


getCurrentPageView : Page -> Element Msg
getCurrentPageView currentPage =
    case currentPage of
        Views.Indexes ->
            IndexesView.view |> Element.map IndexesViewMsg

        Views.Settings ->
            SettingsView.view |> Element.map SettingsViewMsg

        Views.Search ->
            SearchView.view |> Element.map SearchViewMsg

        Views.Stats ->
            StatsView.view |> Element.map StatsViewMsg

        Views.Documents ->
            DocumentsView.view |> Element.map DocumentsViewMsg

        Views.Keys ->
            KeysView.view |> Element.map KeysViewMsg

        Views.Tasks ->
            TasksView.view |> Element.map TasksViewMsg



--|> Element.map Msg
