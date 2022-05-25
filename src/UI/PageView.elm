module UI.PageView exposing (..)

import Element exposing (..)
import UI.PageViews.Documents as DocumentsView
import UI.PageViews.Indexes as IndexesView
import UI.PageViews.Keys as KeysView
import UI.PageViews.Search as SearchView
import UI.PageViews.Settings as SettingsView
import UI.PageViews.Stats as StatsView
import UI.PageViews.Tasks as TasksView
import UI.Pages as Views exposing (Page)



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

        Views.Settings s ->
            SettingsView.view s |> Element.map SettingsViewMsg

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
