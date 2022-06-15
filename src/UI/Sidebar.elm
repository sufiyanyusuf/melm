module UI.Sidebar exposing (Model, Msg(..), sidebarView)

-- import UI.Pages exposing (Page(..))

import Element exposing (..)
import Element.Background as Background
import Element.Border exposing (rounded)
import Element.Events
import Element.Input exposing (OptionState(..))
import UI.Icons exposing (Icon(..), Style(..), buildIcon)
import UI.Pages
import UI.Styles



-- Msg


type Msg
    = SelectPage UI.Pages.Page
    | ShowIndices



-- Model


type alias Model =
    { pages : List UI.Pages.Page, selectedPage : UI.Pages.Page }



-- View


sidebarView : Model -> Element Msg
sidebarView model =
    Element.column
        [ width (px 240)
        , padding 12
        , scrollbarY
        ]
        [ dropDown "Suggestions" []
        , Element.table
            [ width fill
            , height fill
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
        ]


dropDown : String -> List String -> Element Msg
dropDown v l =
    el
        [ width fill ]
        (row
            (List.concat
                [ [ Element.Events.onClick ShowIndices
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

                -- , addIf isSelected <| Background.color UI.Styles.color.primary200
                ]
            )
            [ buildIcon UI.Icons.Pin Outline
            , paragraph [ paddingEach { top = 0, left = 8, bottom = 0, right = 0 } ]
                [ el
                    (UI.Styles.getTypographicStyleFor UI.Styles.Body)
                    (text v)
                ]
            ]
        )


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
                , addIf isSelected <| Background.color UI.Styles.color.primary200
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

        UI.Pages.Documents _ ->
            "Documents"

        UI.Pages.Synonyms _ ->
            "Synonyms"

        UI.Pages.StopWords _ ->
            "Stop Words"

        UI.Pages.Attributes _ ->
            "Attributes"


getPageIcon : UI.Pages.Page -> UI.Icons.Style -> Element msg
getPageIcon page style =
    case page of
        UI.Pages.Settings _ ->
            UI.Icons.buildIcon SettingsGear style

        UI.Pages.Documents _ ->
            UI.Icons.buildIcon Documents style

        UI.Pages.Synonyms _ ->
            UI.Icons.buildIcon Dictionary style

        UI.Pages.StopWords _ ->
            UI.Icons.buildIcon Block style

        UI.Pages.Attributes _ ->
            UI.Icons.buildIcon Switches style


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
