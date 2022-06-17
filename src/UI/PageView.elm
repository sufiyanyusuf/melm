module UI.PageView exposing (..)

import Element exposing (..)
import Element.Background
import UI.PageViews.Attributes as Attributes
import UI.PageViews.Documents as DocumentsView
import UI.PageViews.Indexes as IndexesView
import UI.PageViews.Search as SearchView
import UI.PageViews.Settings as SettingsView
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
    | DocumentsViewMsg DocumentsView.Msg
    | StopWordsViewMsg StopWords.Msg
    | SynonymsViewMsg SynonymsView.Msg
    | AttributesViewMsg Attributes.Msg



-- View


view : Page -> Element Msg
view currentPage =
    el
        [ height fill
        , width fill
        , Element.Background.color UI.Styles.color.gray100
        , padding 1
        ]
        (getCurrentPageView currentPage)


getCurrentPageView : Page -> Element Msg
getCurrentPageView currentPage =
    case currentPage of
        Views.Settings s ->
            SettingsView.view s |> Element.map SettingsViewMsg

        Views.Documents m ->
            DocumentsView.view m |> Element.map DocumentsViewMsg

        Views.Synonyms s ->
            SynonymsView.view s |> Element.map SynonymsViewMsg

        Views.StopWords m ->
            StopWords.view m |> Element.map StopWordsViewMsg

        Attributes m ->
            Attributes.view m |> Element.map AttributesViewMsg



--|> Element.map Msg
