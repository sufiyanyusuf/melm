module UI.Sidebar exposing (Model, Msg(..), init, sidebarView, update)

import Element exposing (..)
import Element.Background as Background
import Element.Border exposing (rounded)
import Element.Events
import Element.Font as Font
import Element.Input exposing (OptionState(..))
import UI.Components.Dropdown as Dropdown
import UI.Icons exposing (Icon(..), Style(..))
import UI.PageViews.Documents as Documents
import UI.Pages
import UI.Styles exposing (Config, color)
import Utils exposing (addIf)



-- Msg


type Msg
    = SelectPage UI.Pages.Page
    | DropdownMsg Dropdown.Msg



-- Model


type alias Model =
    { pages : List UI.Pages.Page, selectedPage : UI.Pages.Page, dropDown : Dropdown.Model }


init : Model
init =
    Model [ UI.Pages.Documents Documents.init ]
        (UI.Pages.Documents Documents.init)
        Dropdown.init



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DropdownMsg m ->
            let
                ( d, _ ) =
                    Dropdown.update m model.dropDown
            in
            ( { model | dropDown = d }, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- View


sidebarView : Model -> Config -> Element Msg
sidebarView model config =
    Element.column
        [ width (px 240)
        , height fill
        , padding 12
        , Element.inFront
            (Dropdown.view model.dropDown config |> Element.map DropdownMsg)
        , scrollbarY
        , Background.color (UI.Styles.color config).white
        ]
        [ Element.table
            [ width fill
            , height fill
            , paddingXY 0 64
            ]
            { data = model.pages
            , columns =
                [ { header = Element.none
                  , width = fill
                  , view =
                        \page -> sidebarListItemView (getPageTitle page) (model.selectedPage == page) page config
                  }
                ]
            }
        ]


sidebarListItemView : String -> Bool -> UI.Pages.Page -> Config -> Element Msg
sidebarListItemView title isSelected page config =
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
                  , Element.mouseOver <|
                        [ if isSelected then
                            Background.color (UI.Styles.color config).primary200

                          else
                            Background.color (UI.Styles.color config).gray200
                        ]
                  ]
                , addIf isSelected <| Background.color (UI.Styles.color config).primary100
                ]
            )
            [ getPageIcon page (getIconStyle isSelected) config (UI.Styles.color config).primary100
            , paragraph [ paddingEach { top = 0, left = 8, bottom = 4, right = 0 } ]
                [ el
                    (List.concat
                        [ UI.Styles.getTypographicStyleFor UI.Styles.Body config
                        , [ Font.color (UI.Styles.color config).gray500 ]
                        , addIf isSelected <| Font.color (UI.Styles.color config).primary500
                        ]
                    )
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


getPageIcon : UI.Pages.Page -> UI.Icons.Style -> Config -> color -> Element msg
getPageIcon page style config color =
    case page of
        UI.Pages.Settings _ ->
            UI.Icons.buildIcon SettingsGear style config color

        UI.Pages.Documents _ ->
            UI.Icons.buildIcon Documents style config color

        UI.Pages.Synonyms _ ->
            UI.Icons.buildIcon Dictionary style config color

        UI.Pages.StopWords _ ->
            UI.Icons.buildIcon Block style config color

        UI.Pages.Attributes _ ->
            UI.Icons.buildIcon Switches style config color


getIconStyle : Bool -> UI.Icons.Style
getIconStyle isSelected =
    if isSelected then
        Filled

    else
        Outline
