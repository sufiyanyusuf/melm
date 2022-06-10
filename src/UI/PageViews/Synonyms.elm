module UI.PageViews.Synonyms exposing (..)

import Api.Routes.Main exposing (..)
import Dict exposing (Dict)
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
        , padding 20
        ]
        [ el
            (UI.Styles.getTypographicStyleFor UI.Styles.H1)
            (text "Synonyms")
        , UI.Elements.spacer UI.Styles.MD
        , Element.table
            [ width fill
            , height fill
            , scrollbarY
            , spacing 20
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
        , toolbarView model
        ]


toolbarView : Model -> Element Msg
toolbarView model =
    Element.row
        [ Element.width Element.shrink
        , padding 12
        ]
        [ UI.Elements.button "New" New
        , UI.Elements.spacer UI.Styles.SM
        , UI.Elements.button "Save" Sync
        ]
