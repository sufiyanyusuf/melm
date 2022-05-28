module UI.Sidebar exposing (Model, Msg(..), sidebarView)

import Element exposing (..)
import Element.Background as Background
import Element.Border exposing (rounded)
import Element.Events
import Element.Input exposing (OptionState(..))
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
            [ paragraph []
                [ el
                    (UI.Styles.getTypographicStyleFor UI.Styles.Body)
                    (text title)
                ]
            ]
        )


getPageTitle : UI.Pages.Page -> String
getPageTitle page =
    case page of
        UI.Pages.Indexes _ ->
            "Indexes"

        UI.Pages.Settings _ ->
            "Settings"

        UI.Pages.Search ->
            "Search"

        UI.Pages.Stats ->
            "Stats"

        UI.Pages.Documents _ ->
            "Documents"

        UI.Pages.Keys ->
            "Keys"

        UI.Pages.Tasks ->
            "Tasks"


addIf : Bool -> Attribute msg -> List (Attribute msg)
addIf isNeed attr =
    if isNeed then
        [ attr ]

    else
        []
