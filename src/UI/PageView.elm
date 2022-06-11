module UI.PageView exposing (..)

import Element exposing (..)
import Element.Background
import UI.PageViews.Attributes as Attributes
import UI.PageViews.Documents as DocumentsView
import UI.PageViews.Indexes as IndexesView
import UI.PageViews.Search as SearchView
import UI.PageViews.Settings as SettingsView
import UI.PageViews.Stats as StatsView
import UI.PageViews.StopWords as StopWords
import UI.PageViews.Synonyms as SynonymsView
import UI.PageViews.Tasks as TasksView
import UI.Pages as Views exposing (Page(..))
import UI.Styles



-- Msg


type Msg
    = IndexesViewMsg IndexesView.Msg
    | SettingsViewMsg SettingsView.Msg
    | SearchViewMsg SearchView.Msg
    | StatsViewMsg StatsView.Msg
    | DocumentsViewMsg DocumentsView.Msg
    | TasksViewMsg TasksView.Msg
    | StopWordsViewMsg StopWords.Msg
    | SynonymsViewMsg SynonymsView.Msg
    | AttributesViewMsg Attributes.Msg



-- View


view : Page -> Element Msg
view currentPage =
    el
        [ height fill
        , width fill

        -- , scrollbarY
        , paddingEach { top = 40, bottom = 0, left = 120, right = 0 }
        , Element.Background.color UI.Styles.color.gray100
        ]
        (getCurrentPageView currentPage)


getCurrentPageView : Page -> Element Msg
getCurrentPageView currentPage =
    case currentPage of
        Views.Settings s ->
            SettingsView.view s |> Element.map SettingsViewMsg

        Views.Stats ->
            StatsView.view |> Element.map StatsViewMsg

        Views.Documents m ->
            DocumentsView.view m |> Element.map DocumentsViewMsg

        Views.Tasks ->
            TasksView.view |> Element.map TasksViewMsg

        Views.RankingRules ->
            Debug.todo "branch 'RankingRules' not implemented"

        Views.Synonyms s ->
            SynonymsView.view s |> Element.map SynonymsViewMsg

        Views.StopWords m ->
            StopWords.view m |> Element.map StopWordsViewMsg

        Attributes m ->
            Attributes.view m |> Element.map AttributesViewMsg



--|> Element.map Msg
