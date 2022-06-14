module UI.PageViews.Attributes exposing (..)

import Element exposing (..)
import Element.Background
import Element.Border
import Request exposing (..)
import UI.Elements exposing (syncIndicator)
import UI.PageViews.Settings exposing (Msg(..))
import UI.Styles exposing (Size(..))


type Msg
    = X Bool
    | Toggle Attribute AttributeType
    | Save


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
            [ spacing 20
            , paddingEach { top = 20, bottom = 0, left = 0, right = 240 }
            ]
            [ cardView model.displayed Displayed
            , cardView model.searchable Searchable
            , cardView model.filterable Filterable
            , cardView model.sortable Sortable
            , cardView model.distinct Distinct
            ]
        , UI.Elements.spacer UI.Styles.LG
        , toolbarView model
        , UI.Elements.spacer UI.Styles.MD
        ]


cardView : List Attribute -> AttributeType -> Element Msg
cardView model attrType =
    let
        title =
            case attrType of
                Displayed ->
                    "Displayed"

                Sortable ->
                    "Sortable"

                Searchable ->
                    "Searchable"

                Filterable ->
                    "Filterable"

                Distinct ->
                    "Distinct"
    in
    Element.column
        [ Element.Background.color UI.Styles.color.white
        , Element.Border.rounded 12
        , padding 24
        , Element.width (px 320)
        ]
        ([ el (UI.Styles.getTypographicStyleFor UI.Styles.H2) (text title)
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
            , UI.Elements.spacer UI.Styles.XS
            , syncIndicator model.requestStatus (model.saved /= model.enabled)
            , UI.Elements.spacer UI.Styles.FILL
            , UI.Elements.switch model.enabled (Toggle model attrType)
            ]
        ]


toolbarView : Model -> Element Msg
toolbarView _ =
    Element.row
        [ Element.width Element.shrink
        ]
        [ UI.Elements.button "Save" Save
        , UI.Elements.spacer UI.Styles.SM
        ]


type alias Attribute =
    { title : String
    , enabled : Bool
    , saved : Bool
    , requestStatus : RequestStatus
    }


init : Model
init =
    buildMockModelFromAttributes [ "attr a", "attr b", "attr c" ]


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
    | Searchable
    | Filterable
    | Distinct


buildModelFromResponse : AttributeType -> List String -> Model -> Model
buildModelFromResponse a r m =
    case a of
        Displayed ->
            if r == [ "*" ] then
                { m | displayed = List.map (\x -> { x | enabled = True }) m.displayed }

            else
                { m
                    | displayed =
                        List.map
                            (\x ->
                                if List.member x.title r then
                                    { x | enabled = True, saved = True }

                                else
                                    { x | enabled = False, saved = False }
                            )
                            m.displayed
                }

        Sortable ->
            if r == [ "*" ] then
                { m | sortable = List.map (\x -> { x | enabled = True }) m.sortable }

            else
                { m
                    | sortable =
                        List.map
                            (\x ->
                                if List.member x.title r then
                                    { x | enabled = True, saved = True }

                                else
                                    { x | enabled = False, saved = False }
                            )
                            m.sortable
                }

        Searchable ->
            if r == [ "*" ] then
                { m | searchable = List.map (\x -> { x | enabled = True }) m.searchable }

            else
                { m
                    | searchable =
                        List.map
                            (\x ->
                                if List.member x.title r then
                                    { x | enabled = True, saved = True }

                                else
                                    { x | enabled = False, saved = False }
                            )
                            m.searchable
                }

        Filterable ->
            if r == [ "*" ] then
                { m | filterable = List.map (\x -> { x | enabled = True }) m.filterable }

            else
                { m
                    | filterable =
                        List.map
                            (\x ->
                                if List.member x.title r then
                                    { x | enabled = True, saved = True }

                                else
                                    { x | enabled = False, saved = False }
                            )
                            m.filterable
                }

        Distinct ->
            { m
                | distinct =
                    List.map
                        (\x ->
                            if List.member x.title r then
                                { x | enabled = True, saved = True }

                            else
                                { x | enabled = False, saved = False }
                        )
                        m.distinct
            }


getDistinctAttr : List Attribute -> Maybe String
getDistinctAttr m =
    List.filter (\x -> x.enabled == True) m
        |> List.map (\x -> x.title)
        |> List.head


updateAttributes : Model -> List Attribute -> AttributeType -> Model
updateAttributes model attrs attrType =
    case attrType of
        Displayed ->
            { model | displayed = attrs }

        Sortable ->
            { model | sortable = attrs }

        Searchable ->
            { model | searchable = attrs }

        Filterable ->
            { model | filterable = attrs }

        Distinct ->
            { model | distinct = attrs }


updateSyncStatusState : List Attribute -> RequestStatus -> List Attribute
updateSyncStatusState model status =
    List.map
        (\c ->
            if c.saved /= c.enabled then
                case status of
                    Success ->
                        { c | requestStatus = status, saved = c.enabled }

                    _ ->
                        { c | requestStatus = status }

            else
                c
        )
        model



-- MOCK


buildMockModelFromAttributes : List String -> Model
buildMockModelFromAttributes l =
    { displayed =
        List.map
            (\x ->
                { title = x
                , enabled = True
                , saved = True
                , requestStatus = NoRequest
                }
            )
            l
    , sortable =
        List.map
            (\x ->
                { title = x
                , enabled = True
                , saved = True
                , requestStatus = NoRequest
                }
            )
            l
    , searchable =
        List.map
            (\x ->
                { title = x
                , enabled = True
                , saved = True
                , requestStatus = NoRequest
                }
            )
            l
    , filterable =
        List.map
            (\x ->
                { title = x
                , enabled = True
                , saved = True
                , requestStatus = NoRequest
                }
            )
            l
    , distinct =
        List.map
            (\x ->
                { title = x
                , enabled = False
                , saved = False
                , requestStatus = NoRequest
                }
            )
            l
    }
