module UI.PageViews.Synonyms exposing (..)

import Api.Routes.Main exposing (..)
import Array
import Element exposing (..)
import Request exposing (..)
import UI.Components.SynonymCard as SynonymCard exposing (Msg(..))
import UI.Components.Toolbar
import UI.Elements
import UI.Styles exposing (Config)
import Utils


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        New ->
            addNew model

        CardViewMsg m ->
            case m of
                Remove i ->
                    update (Delete i) model

                _ ->
                    let
                        synonymCards =
                            model.synonymCards
                                |> List.map (\c -> SynonymCard.update m c)
                                |> List.map (\( a, _ ) -> a)
                    in
                    ( { model | synonymCards = synonymCards }, Cmd.none )

        Delete index ->
            let
                arr =
                    Array.fromList model.synonymCards

                dq =
                    model.deletionQueue

                udq =
                    case Array.get index arr of
                        Just e ->
                            dq ++ [ e ]

                        Nothing ->
                            dq

                uarr =
                    Utils.remove index arr
            in
            ( { model | synonymCards = Array.toList uarr, deletionQueue = udq }, Cmd.none )

        Purge ->
            ( init, Cmd.none )

        _ ->
            ( model, Cmd.none )


type Msg
    = CardViewMsg SynonymCard.Msg
    | Sync
    | New
    | NoAction
    | Reset
    | Delete Int
    | Purge


type alias Model =
    { synonymCards : List SynonymCard.Model
    , deletionQueue : List SynonymCard.Model
    }


init : Model
init =
    { synonymCards = [], deletionQueue = [] }


addNew : Model -> ( Model, Cmd Msg )
addNew model =
    ( { model
        | synonymCards = model.synonymCards ++ [ SynonymCard.init (List.length model.synonymCards) ]
      }
    , Cmd.none
    )


view : Model -> Config -> Element Msg
view model config =
    Element.column
        [ height fill
        , width fill
        , scrollbarY
        , inFront (toolbarView model config)
        , paddingEach { top = 20, bottom = 12, left = 0, right = 0 }
        ]
        [ UI.Elements.spacer UI.Styles.LG
        , Element.table
            [ width fill
            , height fill
            , scrollbarY
            , spacing 20
            , paddingXY 120 60
            ]
            { data = model.synonymCards
            , columns =
                [ { header = Element.none
                  , width = fill
                  , view =
                        \item -> SynonymCard.view item config |> Element.map CardViewMsg
                  }
                ]
            }
        , UI.Elements.spacer UI.Styles.LG
        ]


isLoading : Model -> Bool
isLoading model =
    (List.map (\x -> x.requestStatus) model.synonymCards
        |> List.filter (\x -> x == Fired)
        |> List.length
    )
        /= 0


getValueChanged : Model -> Bool
getValueChanged model =
    (List.map (\x -> SynonymCard.valueChanged x) model.synonymCards
        |> List.filter (\x -> x == True)
        |> List.length
    )
        /= 0
        || List.length model.deletionQueue
        > 0


toolbarView : Model -> Config -> Element Msg
toolbarView m config =
    let
        toolbarModel =
            { valueChanged = getValueChanged m
            , loading = isLoading m
            , showCreateAction = True
            , title = "Synonyms"
            }
    in
    UI.Components.Toolbar.toolbarView toolbarModel New Sync Reset config


updateSyncStatusState : List SynonymCard.Model -> RequestStatus -> List SynonymCard.Model
updateSyncStatusState model status =
    List.map
        (\c ->
            let
                savedValue =
                    case status of
                        Success ->
                            Just ( c.synonymKey, c.synonymList )

                        _ ->
                            c.saved
            in
            if SynonymCard.valueChanged c then
                { c
                    | requestStatus = status
                    , saved = savedValue
                }

            else
                c
        )
        model
