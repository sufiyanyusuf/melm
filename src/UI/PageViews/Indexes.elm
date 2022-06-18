module UI.PageViews.Indexes exposing (..)

import Api.Routes.Main exposing (IndexesRouteResponseListItem)
import Element exposing (..)
import Element.Background as Background
import Iso8601
import Time exposing (Month(..), toDay, toHour, toMinute, toMonth, toSecond, toYear, utc)
import UI.Elements exposing (spacer)
import UI.Styles exposing (Size(..))


type Msg
    = X


type alias Model =
    { indexes : List IndexesRouteResponseListItem }


init : Model
init =
    { indexes = [] }


view : Model -> Element Msg
view model =
    Element.column
        [ width fill
        , height fill
        , scrollbarY
        , padding 4
        ]
        [ Element.row
            [ width fill ]
            [ el
                (UI.Styles.getTypographicStyleFor UI.Styles.H1)
                (text "Indexes")
            , spacer FILL
            , UI.Elements.button UI.Elements.Subtle "Add" X

            -- , el
            --     (UI.Styles.getTypographicStyleFor UI.Styles.H1)
            --     (text "Indexes")
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
                [ { header = headerView "UID"
                  , width = fill
                  , view =
                        \item -> cellView item.uid
                  }
                , { header = headerView "Title"
                  , width = fill
                  , view =
                        \item -> cellView item.name
                  }
                , { header = headerView "Primary Key"
                  , width = fill
                  , view =
                        \item -> cellView item.primaryKey
                  }
                , { header = headerView "Created"
                  , width = fill
                  , view =
                        \item -> cellView (getFormattedTime item.createdAt)
                  }
                , { header = headerView "Last Updated"
                  , width = fill
                  , view =
                        \item -> cellView (getFormattedTime item.updatedAt)
                  }
                ]
            }
        ]


headerView : String -> Element msg
headerView title =
    Element.column
        [ width fill
        ]
        [ el
            (UI.Styles.getTypographicStyleFor UI.Styles.BodyBold)
            (text title)
        , spacer SM
        , el
            [ height (px 1)
            , Background.color UI.Styles.color.gray300
            , width fill
            ]
            Element.none
        ]


cellView : String -> Element msg
cellView title =
    Element.column
        [ width fill
        ]
        [ spacer SM
        , el
            (UI.Styles.getTypographicStyleFor UI.Styles.Body)
            (text title)
        , spacer SM
        , el
            [ height (px 1)
            , Background.color UI.Styles.color.gray300
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
