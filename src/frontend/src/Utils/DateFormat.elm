module Utils.DateFormat exposing (formatDate)
import Iso8601
import DateFormat
import Time

formatDate : String -> String
formatDate date = 
    case Iso8601.toTime date of
        Ok time ->
            standardFormatter Time.utc time

        Err error->
            ""

standardFormatter : Time.Zone -> Time.Posix -> String
standardFormatter =
    DateFormat.format
        [ DateFormat.monthNameFull
        , DateFormat.text " "
        , DateFormat.dayOfMonthSuffix
        , DateFormat.text ", "
        , DateFormat.yearNumber
        ]
