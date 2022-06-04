module UI.Sidebar exposing (Model, Msg(..), sidebarView)

import Element exposing (..)
import Element.Background as Background
import Element.Border exposing (rounded)
import Element.Events
import Element.Input exposing (OptionState(..))
import UI.Icons exposing (Style(..))
import UI.Pages
import UI.Styles



-- Msg


type Msg
    = SelectPage UI.Pages.Page



-- Model


type alias Model =
    { pages : List UI.Pages.Page, selectedPage : UI.Pages.Page }



-- View


sidebarView : Model -> Element Msg
sidebarView model =
    Element.table
        [ width (px 240)
        , height fill
        , padding 12
        , scrollbarY
        ]
        { data = model.pages
        , columns =
            [ { header = Element.none
              , width = fill
              , view =
                    \page -> sidebarListItemView (getPageTitle page) (model.selectedPage == page) page
              }
            ]
        }


sidebarListItemView : String -> Bool -> UI.Pages.Page -> Element Msg
sidebarListItemView title isSelected page =
    el
        []
        (row
            (List.concat
                [ [ Element.Events.onClick (SelectPage page)
                  , width
                        (fill
                            |> minimum 200
                            |> maximum 320
                        )
                  , padding 8
                  , rounded 4
                  , pointer
                  , Element.mouseOver <| [ Background.color UI.Styles.color.gray300 ]
                  ]
                , addIf isSelected <| Background.color UI.Styles.color.primary100
                ]
            )
            [ getPageIcon page (getIconStyle isSelected)
            , paragraph [ paddingEach { top = 0, left = 8, bottom = 0, right = 0 } ]
                [ el
                    (UI.Styles.getTypographicStyleFor UI.Styles.Body)
                    (text title)
                ]
            ]
        )


getPageTitle : UI.Pages.Page -> String
getPageTitle page =
    case page of
        UI.Pages.Settings _ ->
            "Settings"

        UI.Pages.Stats ->
            "Stats"

        UI.Pages.Documents _ ->
            "Documents"

        UI.Pages.Tasks ->
            "Tasks"

        UI.Pages.RankingRules ->
            "Ranking Rules"

        UI.Pages.Synonyms _ ->
            "Synonyms"

        UI.Pages.StopWords _ ->
            "Stop Words"

        UI.Pages.SearchableAttributes ->
            "Searchable Attributes"

        UI.Pages.DistinctAttributes ->
            "Distinct Attributes"

        UI.Pages.DisplayedAttributes ->
            "Displayed Attributes"


getPageIcon : UI.Pages.Page -> UI.Icons.Style -> Element msg
getPageIcon page style =
    case page of
        UI.Pages.Settings _ ->
            Element.html (UI.Icons.settingsGear style)

        UI.Pages.Stats ->
            Element.html (UI.Icons.pieChart style)

        UI.Pages.Documents _ ->
            Element.html (UI.Icons.documents style)

        UI.Pages.Tasks ->
            Element.html (UI.Icons.checkmark style)

        UI.Pages.RankingRules ->
            Element.html (UI.Icons.arrowUpDown style)

        UI.Pages.Synonyms _ ->
            Element.html (UI.Icons.dictionary style)

        UI.Pages.StopWords _ ->
            Element.html (UI.Icons.block style)

        UI.Pages.SearchableAttributes ->
            Element.html (UI.Icons.documentSearch style)

        UI.Pages.DistinctAttributes ->
            Element.html (UI.Icons.block style)

        UI.Pages.DisplayedAttributes ->
            Element.html (UI.Icons.block style)


getIconStyle : Bool -> UI.Icons.Style
getIconStyle isSelected =
    if isSelected then
        Filled

    else
        Outline


addIf : Bool -> Attribute msg -> List (Attribute msg)
addIf isNeed attr =
    if isNeed then
        [ attr ]

    else
        []
