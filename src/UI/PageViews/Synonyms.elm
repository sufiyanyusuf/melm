module UI.PageViews.Synonyms exposing (..)

import Api.Routes.Main exposing (..)
import Element exposing (..)
import UI.Components.SynonymCard as SynonymCard exposing (Msg(..))
import UI.Elements
import UI.Styles


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        New ->
            addNew model

        Sync ->
            ( model, Cmd.none )

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


type Msg
    = CardViewMsg SynonymCard.Msg
    | Sync
    | New


type alias Model =
    { synonymStates : List SynonymCard.Model
    , indexUid : String
    }


init : String -> Model
init indexUid =
    { synonymStates = [ SynonymCard.init 0 indexUid, SynonymCard.init 1 indexUid ], indexUid = indexUid }


addNew : Model -> ( Model, Cmd Msg )
addNew model =
    ( { model
        | synonymStates = model.synonymStates ++ [ SynonymCard.init (List.length model.synonymStates) model.indexUid ]
      }
    , Cmd.none
    )


view : Model -> Element Msg
view model =
    Element.column
        [ height fill
        , width fill
        , scrollbarY
        ]
        [ el
            (UI.Styles.getTypographicStyleFor UI.Styles.H1)
            (text "Synonyms")
        , UI.Elements.spacer UI.Styles.LG
        , Element.table
            [ width fill
            , height fill
            , scrollbarY
            , spacing 20
            , paddingEach { top = 20, bottom = 0, left = 0, right = 120 }
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
        , toolbarView model
        , UI.Elements.spacer UI.Styles.MD
        ]


toolbarView : Model -> Element Msg
toolbarView _ =
    Element.row
        [ Element.width Element.shrink
        ]
        [ UI.Elements.button "New" New
        , UI.Elements.spacer UI.Styles.SM
        , UI.Elements.button "Save" Sync
        ]


updateSyncStatusState : List SynonymCard.Model -> SynonymCard.RequestStatus -> List SynonymCard.Model
updateSyncStatusState model status =
    List.map
        (\c ->
            if SynonymCard.valueChanged c then
                { c | requestStatus = status }

            else
                c
        )
        model
