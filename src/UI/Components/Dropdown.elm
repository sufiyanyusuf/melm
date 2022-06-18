module UI.Components.Dropdown exposing (Model, Msg(..), init, update, view)

import Element exposing (..)
import Element.Background as Background
import Element.Border exposing (rounded)
import Element.Events
import Request exposing (RequestStatus(..))
import UI.Icons exposing (Icon(..), Style(..))
import UI.Styles exposing (Config, Size(..))
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
        [ { id = "mock"
          , title = "mock"
          }
        ]


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
        , Background.color (UI.Styles.color config).white
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
    in
    Element.row
        (List.concat
            [ [ Element.Events.onClick selectedItem
              , width fill
              , paddingXY 4 10
              , rounded 4
              , pointer
              , Element.mouseOver <| [ Background.color (UI.Styles.color config).gray100 ]
              , Element.Border.width 1
              , Element.Border.color (UI.Styles.color config).gray100
              , Background.color (UI.Styles.color config).gray200
              ]
            , addIf expanded <| Background.color (UI.Styles.color config).gray200
            , addIf expanded <| Element.Border.color (UI.Styles.color config).white
            ]
        )
        [ paragraph [ paddingEach { top = 0, left = 8, bottom = 0, right = 0 } ]
            [ el
                (UI.Styles.getTypographicStyleFor UI.Styles.Body config)
                t
            ]
        ]


dropDownMenu : Bool -> List Item -> Config -> Element Msg
dropDownMenu visible items config =
    if visible then
        Element.column
            [ width fill
            , Element.Border.rounded 6
            , padding 4
            , Element.Border.shadow
                { offset = ( 0, 0 )
                , size = 1
                , blur = 12
                , color = (UI.Styles.color config).gray200
                }
            , Element.Border.width 1
            , Element.Border.color (UI.Styles.color config).gray200
            , Background.color (UI.Styles.color config).gray100
            ]
            (List.map (\item -> dropDownMenuListItem item config) items)

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
              , Element.mouseOver <| [ Background.color (UI.Styles.color config).gray200 ]
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
