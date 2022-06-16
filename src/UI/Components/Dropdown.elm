module UI.Components.Dropdown exposing (Model, Msg(..), init, update, view)

import Element exposing (..)
import Element.Background as Background
import Element.Border exposing (rounded, solid)
import Element.Events exposing (onClick, onLoseFocus)
import Element.Input as Input exposing (OptionState(..))
import Request exposing (RequestStatus(..))
import UI.Icons exposing (Icon(..), Style(..), buildIcon)
import UI.Styles exposing (Size(..))
import Utils exposing (addIf)


type Msg
    = TriggerClicked
    | Select


type alias Model =
    { selectedValue : String
    , expanded : Bool
    , options : List String
    }


init : Model
init =
    Model "Suggestions"
        False
        [ "Index 1"
        , "Index 2"
        , "Index 3"
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TriggerClicked ->
            ( { model | expanded = not model.expanded }, Cmd.none )

        _ ->
            ( { model | expanded = not model.expanded }, Cmd.none )


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
        , dropDownMenu model.expanded model.options Select
        ]


dropDownButton : String -> Bool -> msg -> Element msg
dropDownButton t expanded msg =
    Element.row
        (List.concat
            [ [ Element.Events.onClick msg
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
                (text t)
            ]
        ]


dropDownMenu : Bool -> List String -> msg -> Element msg
dropDownMenu visible l msg =
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
            (List.map (\x -> dropDownMenuListItem x msg) l)

    else
        Element.none


dropDownMenuListItem : String -> msg -> Element msg
dropDownMenuListItem t msg =
    Element.row
        (List.concat
            [ [ Element.Events.onClick msg
              , width fill
              , padding 8
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
                (text t)
            ]
        ]
