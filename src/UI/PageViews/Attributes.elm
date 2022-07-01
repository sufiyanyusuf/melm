module UI.PageViews.Attributes exposing
    ( AttributeType(..)
    , Model
    , Msg(..)
    , getDistinctAttr
    , init
    , seedModelFromAttributeKeys
    , update
    , view
    )

import Element exposing (..)
import Element.Background
import Element.Border
import Request exposing (..)
import UI.Components.Toolbar
import UI.Elements exposing (syncIndicator)
import UI.Icons exposing (Icon(..), Style(..))
import UI.PageViews.Settings exposing (Msg(..))
import UI.Styles exposing (ColorHue(..), ColorIntensity(..), Config, Size(..))


type alias Model =
    { displayed : List Attribute
    , sortable : List Attribute
    , searchable : List Attribute
    , filterable : List Attribute
    , distinct : List Attribute
    }


type alias Attribute =
    { title : String
    , enabled : Bool
    , saved : Bool
    , requestStatus : RequestStatus
    }


view : Model -> Config -> Element Msg
view model config =
    Element.column
        [ height fill
        , width fill
        , paddingEach { top = 20, bottom = 12, left = 0, right = 0 }
        , inFront (toolbarView model config)
        ]
        [ Element.column
            [ width fill
            , height fill
            , scrollbarY
            , paddingXY 120 60
            ]
            [ UI.Elements.spacer UI.Styles.LG
            , Element.wrappedRow
                [ spacing 32
                ]
                [ cardView model.displayed Displayed config
                , cardView model.searchable Searchable config
                , cardView model.filterable Filterable config
                , cardView model.sortable Sortable config
                , cardView model.distinct Distinct config
                ]
            , UI.Elements.spacer UI.Styles.LG
            , UI.Elements.spacer UI.Styles.MD
            ]
        ]


getAllAttrs : Model -> List Attribute
getAllAttrs model =
    List.concat [ model.displayed, model.sortable, model.searchable, model.filterable, model.distinct ]


getAttrs : Model -> AttributeType -> List Attribute
getAttrs model attrType =
    case attrType of
        Displayed ->
            model.displayed

        Searchable ->
            model.searchable

        Sortable ->
            model.sortable

        Filterable ->
            model.filterable

        Distinct ->
            model.distinct


isLoading : Model -> Bool
isLoading model =
    (List.map (\x -> x.requestStatus) (getAllAttrs model)
        |> List.filter (\x -> x == Fired)
        |> List.length
    )
        /= 0


getValueChanged : Model -> Bool
getValueChanged model =
    (List.map (\x -> ( x.saved, x.enabled )) (getAllAttrs model)
        |> List.filter (\( s, e ) -> s /= e)
        |> List.length
    )
        /= 0


toolbarView : Model -> Config -> Element Msg
toolbarView model config =
    let
        toolbarModel =
            { valueChanged = getValueChanged model
            , loading = isLoading model
            , showCreateAction = False
            , title = "Attributes"
            }
    in
    UI.Components.Toolbar.toolbarView toolbarModel None Save Reset config


cardView : List Attribute -> AttributeType -> Config -> Element Msg
cardView model attrType config =
    let
        ( title, icon ) =
            case attrType of
                Displayed ->
                    ( "Displayed", UI.Icons.buildIcon UI.Icons.Displayed Outline config Grayscale I500 )

                Sortable ->
                    ( "Sortable", UI.Icons.buildIcon UI.Icons.Sortable Outline config Grayscale I500 )

                Searchable ->
                    ( "Searchable", UI.Icons.buildIcon UI.Icons.Searchable Outline config Grayscale I500 )

                Filterable ->
                    ( "Filterable", UI.Icons.buildIcon UI.Icons.Filterable Outline config Grayscale I500 )

                Distinct ->
                    ( "Distinct", UI.Icons.buildIcon UI.Icons.Distinct Outline config Grayscale I500 )
    in
    Element.column
        [ Element.Background.color (UI.Styles.color White Generic config)
        , Element.Border.rounded 14
        , padding 24
        , Element.width (px 320)
        ]
        ([ icon
         , UI.Elements.spacer SM
         , el (UI.Styles.getTypographicStyleFor UI.Styles.CardTitle config) (text title)
         , UI.Elements.spacer MD
         ]
            ++ List.map (\x -> cardViewRow x attrType config) model
        )


cardViewRow : Attribute -> AttributeType -> Config -> Element Msg
cardViewRow model attrType config =
    Element.column [ width fill ]
        [ UI.Elements.spacer XS
        , Element.row [ width fill ]
            [ el
                (UI.Styles.getTypographicStyleFor UI.Styles.Body config)
                (text model.title)
            , UI.Elements.spacer UI.Styles.XS
            , syncIndicator model.requestStatus (model.saved /= model.enabled) config
            , UI.Elements.spacer UI.Styles.FILL
            , UI.Elements.switch model.enabled (Toggle model attrType) config
            ]
        ]


init : Model
init =
    seedModelFromAttributeKeys []


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


type Msg
    = Toggle Attribute AttributeType
    | Save
    | Reset
    | None
    | UpdateAttributes (List Attribute) AttributeType
    | HandleResponse AttributeType (List String)
    | UpdateSyncStatusState AttributeType RequestStatus
    | Purge


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateAttributes l t ->
            updateAttributes model l t

        HandleResponse t l ->
            buildModelFromResponse t l model

        UpdateSyncStatusState t r ->
            let
                l =
                    getAttrs model t

                ul =
                    updateSyncStatusState l r
            in
            update (UpdateAttributes ul t) model

        Toggle a t ->
            let
                ul =
                    List.map
                        (\x ->
                            if x.title == a.title then
                                { x | enabled = not x.enabled }

                            else
                                x
                        )
                        (getAttrs model t)
            in
            update (UpdateAttributes ul t) model

        Purge ->
            init

        _ ->
            model



-- MOCK


seedModelFromAttributeKeys : List String -> Model
seedModelFromAttributeKeys l =
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
