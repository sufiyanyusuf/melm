module UI.Sidebar exposing (Msg(..), sidebarView)

import Element exposing (..)
import Element.Background as Background
import Element.Border exposing (rounded)
import Element.Events
import UI.Styles
import UI.Views as Views exposing (Page)



-- Msg


type Msg
    = SelectPage Page



-- View


sidebarView : Page -> Element Msg
sidebarView currentPage =
    Element.column
        [ width (px 240)
        , height fill
        , padding 12
        , scrollbarY
        ]
        [ sidebarListItemView Views.Indexes (currentPage == Views.Indexes)
        , sidebarListItemView Views.Documents (currentPage == Views.Documents)
        , sidebarListItemView Views.Settings (currentPage == Views.Settings)
        , sidebarListItemView Views.Search (currentPage == Views.Search)
        , sidebarListItemView Views.Stats (currentPage == Views.Stats)
        , sidebarListItemView Views.Keys (currentPage == Views.Keys)
        , sidebarListItemView Views.Tasks (currentPage == Views.Tasks)
        ]


sidebarListItemView : Page -> Bool -> Element Msg
sidebarListItemView page isSelected =
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
                  , Element.mouseOver <| [ Background.color UI.Styles.color.lightGrey ]
                  ]
                , addIf isSelected <| Background.color UI.Styles.color.lightBlue
                ]
            )
            [ paragraph []
                [ el
                    (UI.Styles.getTypographicStyleFor UI.Styles.Body)
                    (text (Views.pageTitle page))
                ]
            ]
        )


addIf : Bool -> Attribute msg -> List (Attribute msg)
addIf isNeed attr =
    if isNeed then
        [ attr ]

    else
        []
