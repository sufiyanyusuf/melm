module UI.Components.Dropdown exposing (Model, Msg(..), init, update, view)

import Element exposing (..)
import Element.Background as Background
import Element.Border exposing (rounded)
import Element.Events
import Request exposing (RequestStatus(..))
import UI.Icons exposing (Icon(..), Style(..))
import UI.Styles exposing (Size(..))
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


view : Model -> Element Msg
view model =
    column
        [ padding 12
        , moveDown 8
        , Background.color UI.Styles.color.white
        , Element.scrollbarY
        , Element.height
            (shrink
                |> maximum 320
            )
        , Element.width fill
        , spacing 2
        ]
        [ dropDownButton model.selectedValue model.expanded TriggerClicked
        , dropDownMenu model.expanded model.options
        ]


dropDownButton : Maybe Item -> Bool -> msg -> Element msg
dropDownButton item expanded selectedItem =
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
              , Element.mouseOver <| [ Background.color UI.Styles.color.gray300 ]
              , Element.Border.width 1
              , Element.Border.color UI.Styles.color.gray300
              ]
            , addIf expanded <| Background.color UI.Styles.color.gray300
            , addIf expanded <| Element.Border.color UI.Styles.color.white
            ]
        )
        [ paragraph [ paddingEach { top = 0, left = 8, bottom = 0, right = 0 } ]
            [ el
                (UI.Styles.getTypographicStyleFor UI.Styles.Body)
                t
            ]
        ]


dropDownMenu : Bool -> List Item -> Element Msg
dropDownMenu visible items =
    if visible then
        Element.column
            [ width fill
            , Element.Border.rounded 6
            , padding 4
            , Element.Border.shadow
                { offset = ( 0, 0 )
                , size = 0
                , blur = 15
                , color = UI.Styles.color.gray300
                }
            ]
            (List.map (\item -> dropDownMenuListItem item) items)

    else
        Element.none


dropDownMenuListItem : Item -> Element Msg
dropDownMenuListItem item =
    Element.row
        (List.concat
            [ [ Element.Events.onClick (Select item)
              , width fill
              , paddingEach { top = 8, bottom = 10, left = 8, right = 8 }
              , rounded 4
              , pointer
              , Element.mouseOver <| [ Background.color UI.Styles.color.gray300 ]
              ]

            -- , addIf isSelected <| Background.color UI.Styles.color.primary200
            ]
        )
        [ paragraph [ paddingEach { top = 0, left = 8, bottom = 0, right = 0 } ]
            [ el
                (UI.Styles.getTypographicStyleFor UI.Styles.Body)
                (text item.title)
            ]
        ]
