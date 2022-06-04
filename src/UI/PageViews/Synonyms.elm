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
            let
                synonymCards =
                    model.synonymCards
                        |> List.map (\c -> SynonymCard.update m c)
                        |> List.map (\( a, _ ) -> a)
            in
            ( { model | synonymCards = synonymCards }, Cmd.none )

        _ ->
            ( model, Cmd.none )


type Msg
    = Save SynonymCard.Model
    | CardViewMsg SynonymCard.Msg


type alias Model =
    { synonymCards : List SynonymCard.Model
    }


init : Model
init =
    { synonymCards = [ SynonymCard.init 0, SynonymCard.init 1 ] }


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
            { data = model.synonymCards
            , columns =
                [ { header = Element.none
                  , width = fill
                  , view =
                        \item -> SynonymCard.cardView item |> Element.map CardViewMsg
                  }
                ]
            }
        ]