module UI.PageViews.Indexes exposing (..)

import Api.Routes.Main exposing (IndexesRouteResponseListItem)
import Element exposing (..)
import Element.Background as Background
import Iso8601
import Time exposing (Month(..), toDay, toHour, toMinute, toMonth, toSecond, toYear, utc)
import UI.Elements exposing (spacer)
import UI.Styles exposing (Config, Size(..))


type Msg
    = X


type alias Model =
    { indexes : List IndexesRouteResponseListItem }


init : Model
init =
    { indexes = [] }


view : Model -> Config -> Element Msg
view model config =
    Element.column
        [ width fill
        , height fill
        , scrollbarY
        , padding 4
        ]
        [ Element.row
            [ width fill ]
            [ el
                (UI.Styles.getTypographicStyleFor UI.Styles.H1 config)
                (text "Indexes")
            , spacer FILL
            , UI.Elements.button UI.Elements.Subtle "Add" X config
            ]
        , spacer XL
        , spacer XL
        , Element.table
            [ width fill
            , height fill
            , scrollbarY
            ]
            { data = model.indexes
            , columns =
                [ { header = headerView "UID" config
                  , width = fill
                  , view =
                        \item -> cellView item.uid config
                  }
                , { header = headerView "Title" config
                  , width = fill
                  , view =
                        \item -> cellView item.name config
                  }
                , { header = headerView "Primary Key" config
                  , width = fill
                  , view =
                        \item -> cellView item.primaryKey config
                  }
                , { header = headerView "Created" config
                  , width = fill
                  , view =
                        \item -> cellView (getFormattedTime item.createdAt) config
                  }
                , { header = headerView "Last Updated" config
                  , width = fill
                  , view =
                        \item -> cellView (getFormattedTime item.updatedAt) config
                  }
                ]
            }
        ]


headerView : String -> Config -> Element msg
headerView title config =
    Element.column
        [ width fill
        ]
        [ el
            (UI.Styles.getTypographicStyleFor UI.Styles.BodyBold config)
            (text title)
        , spacer SM
        , el
            [ height (px 1)
            , Background.color (UI.Styles.color config).gray300
            , width fill
            ]
            Element.none
        ]


cellView : String -> Config -> Element msg
cellView title config =
    Element.column
        [ width fill
        ]
        [ spacer SM
        , el
            (UI.Styles.getTypographicStyleFor UI.Styles.Body config)
            (text title)
        , spacer SM
        , el
            [ height (px 1)
            , Background.color (UI.Styles.color config).gray300
            , width fill
            ]
            Element.none
        ]


getFormattedTime : String -> String
getFormattedTime time =
    case Iso8601.toTime time of
        Err _ ->
            time

        Ok t ->
            formatTime t


formatTime : Time.Posix -> String
formatTime time =
    String.fromInt (toDay utc time)
        ++ " "
        ++ getMonth (toMonth utc time)
        ++ " "
        ++ String.fromInt (toYear utc time)
        ++ ", "
        ++ formatHourMinute (toHour utc time) (toMinute utc time)


formatHourMinute : Int -> Int -> String
formatHourMinute hour minute =
    if hour > 12 then
        String.fromInt (hour - 12) ++ ":" ++ String.fromInt minute ++ " PM"

    else
        String.fromInt hour ++ ":" ++ String.fromInt minute ++ " AM"


getMonth : Time.Month -> String
getMonth month =
    case month of
        Jan ->
            "Jan"

        Feb ->
            "Feb"

        Mar ->
            "Mar"

        Apr ->
            "Apr"

        May ->
            "May"

        Jun ->
            "Jun"

        Jul ->
            "Jul"

        Aug ->
            "Aug"

        Sep ->
            "Sep"

        Oct ->
            "Oct"

        Nov ->
            "Nov"

        Dec ->
            "Dec"
