module Main exposing (..)

import Api exposing (..)
import Browser
import Element exposing (..)
import Html exposing (Html)
import UI.PageView as PageView
import UI.Sidebar as Sidebar
import UI.Styles exposing (..)
import UI.Views as Views



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            { selectedPage = Views.Indexes }
    in
    ( model, Cmd.none )



-- MODEL


type alias Model =
    { selectedPage : Views.Page }



-- VIEW


view : Model -> Html Msg
view model =
    Element.layout [ width fill, height fill, padding 20 ]
        (Element.row
            [ width fill, spacing 20, height fill ]
            [ Sidebar.sidebarView model.selectedPage |> Element.map SidebarMsg
            , PageView.view model.selectedPage |> Element.map PageViewMsg
            ]
        )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SidebarMsg sidebarMsg ->
            handleSidebarSelection model sidebarMsg

        PageViewMsg _ ->
            ( model, Cmd.none )


handleSidebarSelection : Model -> Sidebar.Msg -> ( Model, Cmd Msg )
handleSidebarSelection model sidebarMsg =
    let
        selectedPage =
            case sidebarMsg of
                Sidebar.SelectPage p ->
                    p
    in
    ( { model | selectedPage = selectedPage }, Cmd.none )



-- MSG


type Msg
    = SidebarMsg Sidebar.Msg
    | PageViewMsg PageView.Msg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch []
