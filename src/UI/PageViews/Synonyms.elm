module UI.PageViews.Synonyms exposing (..)

import Api.Routes.Main exposing (..)
import Element exposing (..)
import Request exposing (..)
import UI.Components.SynonymCard as SynonymCard exposing (Msg(..))
import UI.Components.Toolbar
import UI.Elements
import UI.Styles


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        New ->
            addNew model

        CardViewMsg m ->
            case m of
                _ ->
                    let
                        synonymCards =
                            model.synonymStates
                                |> List.map (\c -> SynonymCard.update m c)
                                |> List.map (\( a, _ ) -> a)
                    in
                    ( { model | synonymStates = synonymCards }, Cmd.none )

        _ ->
            ( model, Cmd.none )


type Msg
    = CardViewMsg SynonymCard.Msg
    | Sync
    | New
    | NoAction


type alias Model =
    { synonymStates : List SynonymCard.Model
    }


init : Model
init =
    { synonymStates = [] }


addNew : Model -> ( Model, Cmd Msg )
addNew model =
    ( { model
        | synonymStates = model.synonymStates ++ [ SynonymCard.init (List.length model.synonymStates) ]
      }
    , Cmd.none
    )


view : Model -> Element Msg
view model =
    Element.column
        [ height fill
        , width fill
        , scrollbarY
        , inFront (toolbarView model)
        , paddingEach { top = 20, bottom = 12, left = 0, right = 0 }
        ]
        [ UI.Elements.spacer UI.Styles.LG
        , Element.table
            [ width fill
            , height fill
            , scrollbarY
            , spacing 20
            , paddingXY 120 40
            ]
            { data = model.synonymStates
            , columns =
                [ { header = Element.none
                  , width = fill
                  , view =
                        \item -> SynonymCard.view item |> Element.map CardViewMsg
                  }
                ]
            }
        , UI.Elements.spacer UI.Styles.LG
        ]


isLoading : Model -> Bool
isLoading model =
    (List.map (\x -> x.requestStatus) model.synonymStates
        |> List.filter (\x -> x == Fired)
        |> List.length
    )
        /= 0


getValueChanged : Model -> Bool
getValueChanged model =
    (List.map (\x -> SynonymCard.valueChanged x) model.synonymStates
        |> List.filter (\x -> x == True)
        |> List.length
    )
        /= 0


toolbarView : Model -> Element Msg
toolbarView m =
    let
        toolbarModel =
            { valueChanged = getValueChanged m
            , loading = isLoading m
            , showCreateAction = True
            , title = "Synonyms"
            }
    in
    UI.Components.Toolbar.toolbarView toolbarModel New Sync NoAction


updateSyncStatusState : List SynonymCard.Model -> RequestStatus -> List SynonymCard.Model
updateSyncStatusState model status =
    List.map
        (\c ->
            if SynonymCard.valueChanged c then
                { c | requestStatus = status }

            else
                c
        )
        model
