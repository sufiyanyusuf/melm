module UI.Components.Dropdown exposing (Model, Msg(..), init, update, view)

import Chart.Attributes exposing (alignMiddle)
import Element exposing (..)
import Element.Background as Background
import Element.Border exposing (rounded)
import Element.Events
import Element.Font
import Request exposing (RequestStatus(..))
import UI.Icons exposing (Icon(..), Style(..))
import UI.Styles exposing (ColorHue(..), ColorIntensity(..), Config, Size(..), applyFontColor)
import Utils exposing (addIf)


type Msg
    = TriggerClicked
    | Select Item


type alias Item =
    { title : String, id : String }


type alias Model =
    { selectedValue : Maybe Item
    , expanded : Bool
    , options : List Item
    }


init : Model
init =
    Model
        Nothing
        False
        []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TriggerClicked ->
            ( { model | expanded = not model.expanded }, Cmd.none )

        Select item ->
            ( { model | expanded = not model.expanded, selectedValue = Just item }, Cmd.none )


view : Model -> Config -> Element Msg
view model config =
    column
        [ padding 12
        , moveDown 8
        , Background.color (UI.Styles.color Clear I100 config)
        , Element.scrollbarY
        , Element.height
            (shrink
                |> maximum 320
            )
        , Element.width fill
        , spacing 2
        ]
        [ dropDownButton model.selectedValue model.expanded TriggerClicked config
        , dropDownMenu model.expanded model.options config
        ]


dropDownButton : Maybe Item -> Bool -> msg -> Config -> Element msg
dropDownButton item expanded selectedItem config =
    let
        t =
            case item of
                Just i ->
                    text i.title

                Nothing ->
                    text "Select an index"

        style =
            if expanded then
                Filled

            else
                Outline

        hue =
            if expanded then
                Primary

            else
                Grayscale

        closeIcon =
            if expanded then
                UI.Icons.Close

            else
                UI.Icons.ChevronDown
    in
    Element.row
        (List.concat
            [ [ Element.Events.onClick selectedItem
              , width fill
              , paddingXY 8 6
              , rounded 6
              , pointer
              , Element.mouseOver <| [ Background.color (UI.Styles.color Grayscale I100 config) ]
              , Element.Border.width 1
              , Element.Border.color (UI.Styles.color Grayscale I100 config)
              , Background.color (UI.Styles.color Grayscale I200 config)
              ]
            , addIf expanded <| Background.color (UI.Styles.color Grayscale I200 config)
            , addIf expanded <| Element.Border.color (UI.Styles.color White Generic config)
            ]
        )
        [ UI.Icons.buildIcon UI.Icons.Folder style config hue I500
        , paragraph [ paddingEach { top = 0, left = 8, bottom = 4, right = 0 } ]
            [ el
                (UI.Styles.getTypographicStyleFor UI.Styles.Body config
                    |> applyFontColor hue I500 config
                )
                t
            ]
        , UI.Icons.buildIcon closeIcon style config hue I500
        ]


dropDownMenu : Bool -> List Item -> Config -> Element Msg
dropDownMenu visible items config =
    if visible then
        Element.column
            [ width fill
            , Element.Border.rounded 6
            , padding 4
            , Element.Border.shadow
                { offset = ( 0, 4 )
                , size = 1
                , blur = 12
                , color = UI.Styles.color Grayscale I200 config
                }
            , Element.Border.width 1
            , Element.Border.color (UI.Styles.color Grayscale I200 config)
            , Background.color (UI.Styles.color Grayscale I100 config)
            ]
            (if List.length items > 0 then
                List.map (\item -> dropDownMenuListItem item config) items

             else
                [ el
                    (UI.Styles.getTypographicStyleFor UI.Styles.Body config
                        ++ [ padding 20
                           , Element.Font.center
                           , Element.width Element.fill
                           ]
                    )
                    (text "No indexes found")

                -- , Element.paragraph
                --     [ Font.center
                --     , Element.spacingXY 0 4
                --     , Element.height Element.shrink
                --     , Element.width Element.fill
                --     ]
                --     [ Element.text "" ]
                ]
            )

    else
        Element.none


dropDownMenuListItem : Item -> Config -> Element Msg
dropDownMenuListItem item config =
    Element.row
        (List.concat
            [ [ Element.Events.onClick (Select item)
              , width fill
              , paddingEach { top = 8, bottom = 10, left = 8, right = 8 }
              , rounded 4
              , pointer
              , Element.mouseOver <| [ Background.color (UI.Styles.color Grayscale I200 config) ]
              ]

            -- , addIf isSelected <| Background.color UI.Styles.color.primary200
            ]
        )
        [ paragraph [ paddingEach { top = 0, left = 8, bottom = 0, right = 0 } ]
            [ el
                (UI.Styles.getTypographicStyleFor UI.Styles.Body config)
                (text item.title)
            ]
        ]
