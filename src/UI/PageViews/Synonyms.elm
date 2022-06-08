module UI.PageViews.Synonyms exposing (..)

import Api.Routes.Main exposing (..)
import Element exposing (..)
import UI.Components.SynonymCard as SynonymCard exposing (Msg(..))
import UI.Elements
import UI.Styles


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CardViewMsg m ->
            case m of
                Save i ->
                    let
                        synonymCards =
                            model.synonymStates
                                |> List.map (\c -> SynonymCard.update m c)
                                |> List.map (\( a, _ ) -> a)
                    in
                    ( { model | synonymStates = synonymCards }, Cmd.none )

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


type alias Model =
    { synonymStates : List SynonymCard.Model
    }


init : Model
init =
    { synonymStates = [ SynonymCard.init 0, SynonymCard.init 1 ] }


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

        -- , cardView (initSynonymCard 0) |> Element.map CardViewMsg
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
                        \item -> SynonymCard.cardView item |> Element.map CardViewMsg
                  }
                ]
            }
        ]
