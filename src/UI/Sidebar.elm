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
import UI.Styles exposing (ColorHue(..), ColorIntensity(..), Config, color)
import Utils exposing (addIf)



-- Msg


type Msg
    = SelectPage UI.Pages.Page
    | DropdownMsg Dropdown.Msg
    | ClearDropdownOptions



-- Model


type alias Model =
    { pages : List UI.Pages.Page
    , selectedPage : UI.Pages.Page
    , dropDown : Dropdown.Model
    }


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

        ClearDropdownOptions ->
            let
                dropdown =
                    let
                        d =
                            model.dropDown

                        options =
                            []

                        selectedValue =
                            Nothing
                    in
                    { d
                        | options = options
                        , selectedValue = selectedValue
                    }
            in
            ( { model | dropDown = dropdown }, Cmd.none )

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
        , Background.color (UI.Styles.color White Generic config)
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
                  , paddingXY 8 6
                  , rounded 4
                  , pointer
                  , Element.mouseOver <|
                        [ if isSelected then
                            Background.color (UI.Styles.color Primary I200 config)

                          else
                            Background.color (UI.Styles.color Grayscale I200 config)
                        ]
                  ]
                , addIf isSelected <| Background.color (UI.Styles.color Primary I100 config)
                ]
            )
            [ getPageIcon page (getIconStyle isSelected) (getIconHue isSelected) config
            , paragraph [ paddingEach { top = 0, left = 8, bottom = 4, right = 0 } ]
                [ el
                    (List.concat
                        [ UI.Styles.getTypographicStyleFor UI.Styles.Body config
                        , [ Font.color (UI.Styles.color Grayscale I500 config) ]
                        , addIf isSelected <| Font.color (UI.Styles.color Primary I500 config)
                        , [ Font.letterSpacing 0 ]
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


getPageIcon : UI.Pages.Page -> UI.Icons.Style -> ColorHue -> Config -> Element msg
getPageIcon page style hue config =
    case page of
        UI.Pages.Settings _ ->
            UI.Icons.buildIcon SettingsGear style config hue I500

        UI.Pages.Documents _ ->
            UI.Icons.buildIcon Documents style config hue I500

        UI.Pages.Synonyms _ ->
            UI.Icons.buildIcon Dictionary style config hue I500

        UI.Pages.StopWords _ ->
            UI.Icons.buildIcon Block style config hue I500

        UI.Pages.Attributes _ ->
            UI.Icons.buildIcon Switches style config hue I500


getIconStyle : Bool -> UI.Icons.Style
getIconStyle isSelected =
    if isSelected then
        Filled

    else
        Outline


getIconHue : Bool -> ColorHue
getIconHue isSelected =
    if isSelected then
        Primary

    else
        Grayscale
