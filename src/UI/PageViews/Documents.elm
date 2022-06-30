module UI.PageViews.Documents exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border exposing (rounded)
import Json.Decode exposing (decodeString, field, int, string)
import Json.Print as Print
import Request exposing (RequestStatus(..))
import UI.Components.Toolbar
import UI.Elements
import UI.Styles exposing (ColorHue(..), ColorIntensity(..), Config)


type Msg
    = Clear
    | None


update : Msg -> Model -> Model
update msg model =
    case msg of
        Clear ->
            { model | documents = [] }

        None ->
            model


type alias Model =
    { documents : List String
    }


init : Model
init =
    { documents = [] }


view : Model -> Config -> Element Msg
view model config =
    Element.column
        [ height fill
        , width fill
        , inFront (toolbarView model config)
        , paddingEach { top = 20, bottom = 12, left = 0, right = 0 }
        ]
        [ UI.Elements.spacer UI.Styles.SM
        , Element.table
            [ width fill
            , height fill
            , scrollbarY
            , spacing 16
            , paddingXY 120 60
            ]
            { data = model.documents
            , columns =
                [ { header = Element.none
                  , width = fill
                  , view =
                        \document ->
                            Element.column
                                (card (UI.Styles.getTypographicStyleFor UI.Styles.Code config) config)
                                [ Element.text
                                    (Maybe.withDefault "fail" (getIdString document "id"))
                                , Element.text
                                    (Result.withDefault "" (Print.prettyString { indent = 4, columns = 1 } document))
                                ]
                  }
                ]
            }
        ]


toolbarView : Model -> Config -> Element Msg
toolbarView _ config =
    let
        toolbarModel =
            { valueChanged = False
            , loading = False
            , showCreateAction = False
            , title = "Documents"
            }
    in
    UI.Components.Toolbar.toolbarView toolbarModel None None None config


card : List (Element.Attr () msg) -> Config -> List (Element.Attr () msg)
card t config =
    t
        ++ [ padding 20
           , rounded 12
           , Background.color (UI.Styles.color White Generic config)
           ]


getIdString : String -> String -> Maybe String
getIdString json key =
    case decodeString (field key string) json of
        Ok v ->
            Just v

        Err _ ->
            Nothing


getIdInt : String -> String -> Maybe Int
getIdInt json key =
    case decodeString (field key int) json of
        Ok v ->
            Just v

        Err _ ->
            Nothing
