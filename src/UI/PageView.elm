module UI.PageView exposing (..)

import Element exposing (..)
import Element.Background
import UI.PageViews.Attributes as Attributes
import UI.PageViews.Documents as DocumentsView
import UI.PageViews.Indexes as IndexesView
import UI.PageViews.Settings as SettingsView
import UI.PageViews.StopWords as StopWords
import UI.PageViews.Synonyms as SynonymsView
import UI.Pages as Views exposing (Page(..))
import UI.Styles exposing (ColorHue(..), ColorIntensity(..), Config)



-- Msg


type Msg
    = SettingsViewMsg SettingsView.Msg
    | DocumentsViewMsg DocumentsView.Msg
    | StopWordsViewMsg StopWords.Msg
    | SynonymsViewMsg SynonymsView.Msg
    | AttributesViewMsg Attributes.Msg



-- View


view : Page -> Config -> Element Msg
view currentPage config =
    el
        [ height fill
        , width fill
        , Element.Background.color (UI.Styles.color Grayscale I100 config)
        , padding 1
        ]
        (getCurrentPageView currentPage config)


getCurrentPageView : Page -> Config -> Element Msg
getCurrentPageView currentPage config =
    case currentPage of
        Views.Settings s ->
            SettingsView.view s config |> Element.map SettingsViewMsg

        Views.Documents m ->
            DocumentsView.view m config |> Element.map DocumentsViewMsg

        Views.Synonyms s ->
            SynonymsView.view s config |> Element.map SynonymsViewMsg

        Views.StopWords m ->
            StopWords.view m config |> Element.map StopWordsViewMsg

        Attributes m ->
            Attributes.view m config |> Element.map AttributesViewMsg



--|> Element.map Msg
