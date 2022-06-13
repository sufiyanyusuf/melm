module UI.PageViews.Attributes exposing (..)

import Element exposing (..)
import Element.Background
import Element.Border
import UI.Elements
import UI.Styles exposing (Size(..))


type Msg
    = X Bool
    | Toggle Attribute AttributeType


view : Model -> Element Msg
view model =
    Element.column
        [ height fill
        , width fill
        , scrollbarY
        ]
        [ el
            (UI.Styles.getTypographicStyleFor UI.Styles.H1)
            (text "Attributes")
        , UI.Elements.spacer UI.Styles.LG
        , Element.wrappedRow
            [ spacing 12
            , paddingEach { top = 20, bottom = 0, left = 0, right = 320 }
            ]
            [ cardView model.displayed Displayed ]
        , UI.Elements.spacer UI.Styles.LG
        ]


cardView : List Attribute -> AttributeType -> Element Msg
cardView model attrType =
    Element.column
        [ Element.Background.color UI.Styles.color.white
        , Element.Border.rounded 12
        , padding 24
        , Element.width (px 320)
        ]
        ([ el (UI.Styles.getTypographicStyleFor UI.Styles.H2) (text "Displayed")
         , UI.Elements.spacer MD
         ]
            ++ List.map (\x -> cardViewRow x attrType) model
        )


cardViewRow : Attribute -> AttributeType -> Element Msg
cardViewRow model attrType =
    Element.column [ width fill ]
        [ UI.Elements.spacer XS
        , Element.row [ width fill ]
            [ el
                (UI.Styles.getTypographicStyleFor UI.Styles.Body)
                (text model.title)
            , UI.Elements.spacer UI.Styles.FILL
            , UI.Elements.switch model.isOn (Toggle model attrType)
            ]
        ]


type alias Attribute =
    { title : String
    , isOn : Bool
    }


init : Model
init =
    buildModelFromAttributes [ "attr a", "attr b", "attr c" ]


type alias Model =
    { displayed : List Attribute
    , sortable : List Attribute
    , searchable : List Attribute
    , filterable : List Attribute
    , distinct : List Attribute
    }


type AttributeType
    = Displayed
    | Sortable
    | Searchabe
    | Filterable
    | Distinct


buildModelFromAttributes : List String -> Model
buildModelFromAttributes l =
    { displayed =
        List.map
            (\x ->
                { title = x
                , isOn = True
                }
            )
            l
    , sortable =
        List.map
            (\x ->
                { title = x
                , isOn = True
                }
            )
            l
    , searchable =
        List.map
            (\x ->
                { title = x
                , isOn = True
                }
            )
            l
    , filterable =
        List.map
            (\x ->
                { title = x
                , isOn = True
                }
            )
            l
    , distinct =
        List.map
            (\x ->
                { title = x
                , isOn = False
                }
            )
            l
    }


buildModelFromResponse : AttributeType -> List String -> Model -> Model
buildModelFromResponse a r m =
    case a of
        Displayed ->
            let
                displayedAttrs =
                    m.displayed
            in
            if r == [ "*" ] then
                { m | displayed = List.map (\x -> { x | isOn = True }) m.displayed }

            else
                { m
                    | displayed =
                        List.map
                            (\x ->
                                if List.member x.title r then
                                    { x | isOn = True }

                                else
                                    { x | isOn = False }
                            )
                            m.displayed
                }

        _ ->
            m


updateAttributes : Model -> List Attribute -> AttributeType -> Model
updateAttributes model attrs attrType =
    case attrType of
        Displayed ->
            { model | displayed = attrs }

        Sortable ->
            { model | sortable = attrs }

        Searchabe ->
            { model | searchable = attrs }

        Filterable ->
            { model | filterable = attrs }

        Distinct ->
            { model | distinct = attrs }
