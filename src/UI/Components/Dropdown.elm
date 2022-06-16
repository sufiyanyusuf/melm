module UI.Components.Dropdown exposing (Model, Msg(..), init, update, view)

import Element exposing (..)
import Element.Background as Background
import Element.Border exposing (rounded)
import Element.Events exposing (onClick, onLoseFocus)
import Element.Input as Input exposing (OptionState(..))
import Request exposing (RequestStatus(..))
import UI.Icons exposing (Icon(..), Style(..), buildIcon)
import UI.Styles exposing (Size(..))


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
            ( model, Cmd.none )


view : Model -> Element Msg
view model =
    column
        [ padding 8
        , moveDown 8
        , Element.Border.shadow
            { offset = ( 0, 0 )
            , size = 0
            , blur = 15
            , color = Element.rgba255 186 189 182 0.6
            }
        , Background.color UI.Styles.color.white
        , Element.Border.rounded 8
        , Element.scrollbarY
        , Element.height
            (shrink
                -- |> minimum 120
                |> maximum 320
            )
        , Element.width fill
        ]
        [ dropDownButton model.selectedValue TriggerClicked
        , dropDownBody model.expanded model.options Select
        ]


dropDownButton : String -> msg -> Element msg
dropDownButton t msg =
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


dropDownBody : Bool -> List String -> msg -> Element msg
dropDownBody visible l msg =
    if visible then
        Element.column
            [ width fill
            ]
            (List.map (\x -> dropDownButton x msg) l)

    else
        Element.none
